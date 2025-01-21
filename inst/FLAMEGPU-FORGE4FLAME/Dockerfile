# Base image https://hub.docker.com/u/rocker/
FROM nvcr.io/nvidia/cuda:12.3.0-devel-ubuntu22.04
LABEL maintainer="Daniele Baccega <daniele.baccega@unito.it>"

RUN apt update \
    && apt install -y build-essential cmake doxygen git unzip \
    && apt install -y wget \
    && apt install -y libglu1-mesa-dev freeglut3-dev mesa-common-dev libxmu-dev libxi-dev libgl-dev libfreetype6-dev libfontconfig1-dev libdevil-dev \
    && apt clean \
    && rm -rf /var/lib/apt/lists/*

ENV TMP_DIR=/tmp
RUN mkdir TMP_DIR

# SDL
# ENV TMP_SDL=${TMP_DIR}/SDL
# RUN wget -nc https://github.com/libsdl-org/SDL/releases/download/release-2.30.2/SDL2-2.30.2.zip \
#     && unzip SDL2-2.30.2.zip -d $TMP_SDL \
#     && rm SDL2-2.30.2.zip
# RUN cd ${TMP_SDL}/SDL2-2.30.2 \
#     && ./configure \
#     && make \
#     && make install

# # GLM
# ENV TMP_GLM=${TMP_DIR}/GLM
# RUN git clone https://github.com/g-truc/glm.git $TMP_GLM
# RUN cmake -DGLM_BUILD_TESTS=OFF -DBUILD_SHARED_LIBS=OFF -B ${TMP_GLM}/build $TMP_GLM \
#     && cmake --build ${TMP_GLM}/build -- all \
#     && cmake --build ${TMP_GLM}/build -- install

# # GLEW
# ENV TMP_GLEW=${TMP_DIR}/glew
# RUN git clone https://github.com/nigels-com/glew.git $TMP_GLEW
# RUN make extensions -C ${TMP_GLEW} \
#     && make -C ${TMP_GLEW} \
#     && make install -C ${TMP_GLEW} \
#     && make clean -C ${TMP_GLEW}

# Create flamegpu2 directory
RUN mkdir /home/docker; chmod -R 777 /home/docker
RUN mkdir /home/docker/flamegpu2; chmod 777 /home/docker/flamegpu2

WORKDIR /home/docker/flamegpu2/

# COPY abm.sh .
# COPY build.sh .
# COPY clean.sh .
# COPY CMakeLists.txt .
# COPY generate_configuration.sh .
# COPY run.sh .