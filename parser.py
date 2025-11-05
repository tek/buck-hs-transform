#!/usr/bin/env python3

import os
import sys
import glob
import json
import shutil
import argparse
import itertools
from pathlib import Path

import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)

import tree_sitter
import tree_sitter_haskell

HS_LANGUAGE = tree_sitter.Language(tree_sitter_haskell.language())

def find_files(root_dir, dirs):
    rp = Path(root_dir)
    for dir in (['.'] if len(dirs) == 0 else dirs):
        for glob in ('{}/*.hs', '{}/**/*.hs'):
            for path in rp.glob(glob.format(dir)):
                yield str(path)

def parse_hs(path, parser):
    with open(path, 'r') as f:
        code = f.read()
    tree = parser.parse(bytes(code, 'utf8'))
    root = tree.root_node

    def node_text(node):
        return code[node.start_byte:node.end_byte]

    module_name = None
    imports = []

    # Find module name in header → module → module_id
    for node in root.children:
        if node.type == 'header':
            for child in node.children:
                if child.type == 'module':
                    module_ids = [node_text(grandchild).strip() for grandchild in child.children if grandchild.type == 'module_id']
                    if module_ids:
                        module_name = '.'.join(module_ids)

    # Find imports in imports → import → module → module_id (only the first module child)
    for node in root.children:
        if node.type == 'imports':
            for imp in node.children:
                if imp.type == 'import':
                    module_node = next((c for c in imp.children if c.type == 'module'), None)
                    if module_node:
                        mod_ids = [node_text(c).strip() for c in module_node.children if c.type == 'module_id']
                        if mod_ids:
                            imports.append('.'.join(mod_ids))

    if module_name == None:
        return None
    else:
        return {
            'module_name': module_name,
            'imports': imports
        }

def write_json(input_root, output_dir, input_path, data):
    rel_path = os.path.relpath(input_path, input_root)
    out_path = os.path.join(output_dir, rel_path)
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    with open(out_path, 'w') as f:
        json.dump(data, f, indent=2)

def process_file(input_root, output_dir, path):
    parser = tree_sitter.Parser(HS_LANGUAGE)
    try:
        data = parse_hs(path, parser)
    except Exception as e:
        print('>>> While parsing {}'.format(path))
        raise e
    if data != None:
        write_json(input_root, output_dir, path, data)

def process_files(input_root, dirs, output_dir):
    for file in find_files(input_root, dirs):
        process_file(input_root, output_dir, file)

def main(options):
    output_dir = options.out
    if os.path.exists(output_dir):
        shutil.rmtree(output_dir)
    os.makedirs(output_dir)
    process_files(options.input_root, options.dirs, output_dir)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Collect Haskell imports')
    parser.add_argument('dirs', help='Restrict to these subdirectories of --in', nargs='*')
    parser.add_argument('--in', required=False, type=Path, help='Root of the Buck project', default='.', dest='input_root')
    parser.add_argument('--out', required=True, type=Path, help='Output directory')
    main(parser.parse_args())
