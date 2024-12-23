FROM ubuntu:latest
RUN apt-get update && apt-get install -y picolisp w3m readline-doc nano
COPY .inputrc /root
RUN mkdir /root/.pil
COPY pil/rc /root/.pil
COPY pil/viprc /root/.pil
WORKDIR /code
RUN /bin/bash
