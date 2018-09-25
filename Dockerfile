FROM ubuntu
# initialize apt and install some basic tools
RUN apt-get update
RUN apt-get install -y vim tree silversearcher-ag
# install core tools for this project
RUN apt-get install -y opam build-essential m4 gcc-multilib
RUN opam init -y && opam install -y dune merlin utop ocp-indent
RUN echo 'eval `opam config env`' >> ~/.bashrc
ENTRYPOINT sleep infinity
