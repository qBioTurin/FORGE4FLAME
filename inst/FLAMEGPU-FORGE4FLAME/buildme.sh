#!/bin/bash

# DNF_CMD=$(which dnf)
# APT_GET_CMD=$(which apt-get)
# YUM_CMD=$(which yum)

# if [[ ! -z $DNF_CMD ]]; then
#   curl -s -L https://nvidia.github.io/libnvidia-container/stable/rpm/nvidia-container-toolkit.repo | sudo tee /etc/yum.repos.d/nvidia-container-toolkit.repo
#   sudo dnf update
#   sudo dnf install -y nvidia-container-toolkit nvidia-docker2
# elif [[ ! -z $APT_GET_CMD ]]; then
#   distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
#   curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg \
#     && curl -s -L https://nvidia.github.io/libnvidia-container/$distribution/libnvidia-container.list | \
#        sed 's#deb https://#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https://#g' | \
#        sudo tee /etc/apt/sources.list.d/nvidia-container-toolkit.list
#   sudo apt update
#   sudo apt-get install -y nvidia-container-toolkit nvidia-docker2
# elif [[ ! -z $YUM_CMD ]]; then
#   sudo yum update
#   sudo yum install -y nvidia-container-toolkit nvidia-docker2
# else
#   echo "Error can't install package."
#   exit 1;
# fi

# sudo nvidia-ctk runtime configure --runtime=docker
# sudo systemctl daemon-reload
# sudo systemctl restart docker

docker build -t danielebaccega/flamegpu2-run .