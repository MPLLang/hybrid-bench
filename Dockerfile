FROM nvidia/cuda:12.4.1-devel-ubuntu20.04

RUN set -e

RUN apt-get update -qq \
 && apt-get install -qq git build-essential libgmp-dev mlton mlton-tools vim curl unzip tmux wget \
 && cd /root && wget https://sourceforge.net/projects/mlton/files/mlton/20210117/mlton-20210117-1.amd64-linux-glibc2.31.tgz \
 && tar -xvzf mlton-20210117-1.amd64-linux-glibc2.31.tgz \
 && cd /root/mlton-20210117-1.amd64-linux-glibc2.31 && make install

ENV TZ=America/New_York
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN echo "tzdata tzdata/Areas select America" | debconf-set-selections && echo "tzdata tzdata/Zones/America select New_York" | debconf-set-selections

RUN git clone https://github.com/MPLLang/mpl.git /root/mpl \
 && cd /root/mpl && git checkout hybrid-sched && make

RUN apt install -y libtinfo-dev zlib1g-dev libffi-dev libncurses5 pkg-config \
 && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=latest \
 BOOTSTRAP_HASKELL_CABAL_VERSION=latest BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
 BOOTSTRAP_HASKELL_ADJUST_BASHRC=P sh

ENV PATH /root/.ghcup/bin:$PATH

RUN git clone https://github.com/diku-dk/futhark.git /root/futhark \
 && cd /root/futhark && make configure && make build && make install

ENV PATH /root/.local/bin:$PATH

RUN git clone https://github.com/diku-dk/smlfut.git /root/smlfut \
 && cd /root/smlfut && MLCOMP=mlton make \
 && MLCOMP=mlton make install \
 && git clone https://github.com/diku-dk/smlpkg.git /root/smlpkg \
 && cd /root/smlpkg && MLCOMP=mlton make clean all \
 && git clone https://github.com/MPLLang/hybrid-bench.git /root/hybrid-bench \
 && cd /root/hybrid-bench/dep && /root/smlpkg/src/smlpkg sync

RUN apt-get install -y python3 jq python3-numpy python3-matplotlib \
 && git clone https://github.com/OpenMathLib/OpenBLAS.git /root/OpenBLAS \
 && cd /root/OpenBLAS && make

ENV CPATH /usr/local/cuda-12.4/targets/x86_64-linux/include:$CPATH

RUN ln -s /usr/local/cuda-12.4/targets/x86_64-linux/lib/libcudart.so /usr/local/cuda-12.4/targets/x86_64-linux/lib/stubs \
 && ln -s /usr/local/cuda-12.4/targets/x86_64-linux/lib/libcudart.a /usr/local/cuda-12.4/targets/x86_64-linux/lib/stubs
