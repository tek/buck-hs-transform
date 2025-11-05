{
  description = "Buck/Haskell dependency skeleton generator dev environment";

  inputs = {
    hix.url = "github:tek/hix/aea37e6c6559586c370dee8043e90c391f08f860";
    fenix = {
      url = "github:nix-community/fenix/9d17341a4f227fe15a0bca44655736b3808e6a03";
      inputs.nixpkgs.follows = "hix/nixpkgs";
    };
  };

  outputs = {hix, fenix, ...}: hix.lib.pro ({config, lib, util, ...}: {

    packages.transform = {
      src = ./packages/transform;
      library = {
        enable = true;
        dependencies = [
          "aeson"
          "bytestring"
          "containers"
          "directory"
          "directory-contents"
          "exon"
          "extra"
          "filepath"
          "filepattern"
          "optparse-applicative"
          "path"
          "path-io"
          "text"
          "transformers"
        ];
      };
      executable = {
        enable = true;
      };
    };

    overrides = {force, hackage, ...}: {
      directory-contents = force;
      exon = hackage "1.7.2.0" "0hg271cvjqm4ps75qpnirq9nvjwpwb03mcbn1a364jrysrj6bg3b";
      incipit-base = hackage "0.6.1.1" "08ybv7j94yyznrxnrh744bi3i1a00sz8bf5ddfs9vfgfhhkrg8fn";
    };

    envs.parse = {config, ...}: let

      inherit (config.toolchain) pkgs;

    in {
      expose.shell = true;
      packages = [];
      package-set.compiler.nixpkgs = "parse";
      buildInputs = [
        pkgs.tree-sitter
        (pkgs.python3.withPackages (py: [
          py.tree-sitter
          py.tree-sitter-grammars.tree-sitter-haskell
        ]))
      ];
    };

    cabal = {

      language = "GHC2021";

      prelude = {
        enable = true;
        package = "incipit-base";
        module = "IncipitBase";
      };

    };

    internal.hixCli.dev = true;

    compiler = "ghc910";

    nixpkgs.parse.source = builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/ca534a76c4afb2bdc07b681dbc11b453bab21af8.tar.gz";
      sha256 = "12dg8slqvwi1nvpi1rs0w6bwhk4yl1hj22gj7z2cdfnqnjabfr3h";
    };

    compilers.buck.source.build = {
      url = "https://gitlab.haskell.org/ghc/ghc";
      version = "9.10.1";
      flavour = "release+split_sections+ipe";
      rev = "8323b4b98a498628021d037253034f648e1ace21";
      hash = "sha256-L/KbAwfYcuPwSJUwqpWRBCCu1SZFYv9q+d96Vq552t4=";
    };

    # ------------------------------------------------------------------------------------------------------------------
    # Buck

    # The environment for the CLI tool `buck`, using the Buck overlay extracted from MWB.
    # `fenix` is a dep of Buck.
    # Exposes a devShell named `buck` that should be used to gain access to the CLI tool.
    envs.buck = {
      package-set.compiler.source = "ghc910";
      expose.shell = true;
      packages = [];
      buildInputs = pkgs: [pkgs.buck2-source];

      package-set.compiler.nixpkgs.overlays = [
        fenix.overlays.default
        (import ./ops/buck/overlay.nix)
      ];
    };

    # The environment for our Buck nixpkgs integration, from which GHC and the package set are taken when exposing them
    # in `outputs.packages` below.
    # Uses our custom GHC build and injects a hook into all Haskell derivations that creates `package.cache` in the
    # store dir, which is needed because Buck supplies individual package DBs to GHC.
    envs.buck-build = {
      packages = [];
      package-set.compiler = "buck";

      overrides = api@{override, ...}: let
        testDeps = import ./ops/test-deps.nix { inherit util; };
      in testDeps.overrides api // {
        __all = override (drv: {
          postInstall = (drv.postInstall or "") + ''
          ghc-pkg recache --package-db $packageConfDir
          '';
        });
      };
    };

    # The interface that Buck expects when loading Nix packages in `toolchains/BUCK` using those `nix.rules.flake`
    # rules.
    # Exposes the toolchain Haskell packages listed in `./ops/ghc-toolchain-libraries.nix` in the attribute
    # `haskellPackages.libs` as well as Python and the GHC compiler derivation.
    outputs.packages = import ./ops/buck/packages.nix { inherit config lib; };

    commands.collect-targets = {
      expose = true;
      env = "buck";
      command = ''
      if [[ -z $1 ]]
      then
        print 'Usage: collect-targets ROOT_TARGET'
        exit 1
      fi
      target=$1

      buck uquery -B --output-format json "kind('haskell_library|export_file', deps('$target'))" > buck.json
      '';
    };

  });

}
