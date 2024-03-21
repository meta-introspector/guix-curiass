#!/bin/bash
set -x
set | grep .
echo hello
cd /mnt/data1/nix/time/2024/03/18/guix-cuirass/
export GUILE_LOAD_COMPILED_PATH=/mnt/data1/nix/time/2024/03/18/guix-cuirass/src
export GUILE_LOAD_PATH=/mnt/data1/nix/time/2024/03/18/guix-cuirass/src
export CUIRASS_DATADIR=/mnt/data1/nix/time/2024/03/18/guix-cuirass/src
export CUIRASS_STATE_DIRECTORY=/mnt/data1/nix/time/2024/03/18/guix-cuirass/var
#export PATH=/mnt/data1/nix/time/2024/03/18/guix-cuirass/bin:/nix/store/032wiarm65zp3bh9ak3dz2sqcr3n8g70-bash-interactive-5.2p26/bin:/nix/store/arc3273y873j72pqyw0iisc5f3cri8zs-opam-2.1.5/bin:/nix/store/61k4vacdclrjmdzd6r651ns9bpj2h3nq-git-2.42.0/bin:/nix/store/zf0ji4phfrw2kqkc2bi6z91shzdbbaji-coq-8.18.0/bin:/nix/store/izb8i2vyrvd45w5v3mxllzd5gds7nkz4-elixir-1.15.7/bin:/nix/store/6n1hz0wv2ywj4sv8ja5whbf59dfbvpsh-emacs-with-packages-28.2/bin:/nix/store/7ywggws38m4788rm8fvbcsmyddnq0q3m-patchelf-0.15.0/bin:/nix/store/rk067yylvhyb7a360n8k1ps4lb4xsbl3-coreutils-9.3/bin:/nix/store/q7x6rjg6ya1gsg068fxj1sgf1k2n144n-findutils-4.9.0/bin:/nix/store/r9jlsp2szh2c1ns2milpc0qynqq28hq8-diffutils-3.10/bin:/nix/store/29w8hg0fis0pl3j4d3v0p02aicyw10lv-gnused-4.9/bin:/nix/store/r1lp9kxlrc6h7vrba90gm6i94s31xvvx-gnugrep-3.11/bin:/nix/store/r9w7hwj3ahip499dlr3zzk6601x2v9kf-gawk-5.2.2/bin:/nix/store/0ybz4c399fq3wiqc1ni0yylcxhs1x1hc-gnutar-1.35/bin:/nix/store/hcrf95x3r60kw71wgwbdybjfcq0ipkpj-gzip-1.13/bin:/nix/store/gfs0a87fzs9msav9ckdnciw3fappzgg2-bzip2-1.0.8-bin/bin:/nix/store/5vyb54sgl2x1ihrdfvj46vi2w1acv9nn-gnumake-4.4.1/bin:/nix/store/r9h133c9m8f6jnlsqzwf89zg9w0w78s8-bash-5.2-p15/bin:/nix/store/dp0pd2rljai6jlp7jb254vmbmyzik4nc-patch-2.7.6/bin:/nix/store/gvdfpnyr6smc7g9wy9fjp44j41j73f4b-xz-5.4.4-bin/bin:/nix/store/g0kc0z0wzs2s4kl377zssyxmmk5z9vim-file-5.45/bin:/gnu/store/n389x58y3wk97agg28219d8gzngzj3i1-profile/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/snap/bin

#./bin/cuirass register
#/usr/local/bin/cuirass $*=
export GUILE_LOAD_PATH=/usr/local/share/guile/site/3.0:/gnu/store/yrc3vrs7b12i9imy22gzhphw65awxkdx-profile/share/guile/site/3.0
export GUILE_LOAD_COMPILED_PATH=/usr/local/lib/guile/3.0/site-ccache:/gnu/store/yrc3vrs7b12i9imy22gzhphw65awxkdx-profile/lib/guile/3.0/site-ccache:/gnu/store/yrc3vrs7b12i9imy22gzhphw65awxkdx-profile/share/guile/site/3.0
/gnu/store/yrc3vrs7b12i9imy22gzhphw65awxkdx-profile/bin/guile --no-auto-compile -e main -s /usr/local/bin/cuirass $*

