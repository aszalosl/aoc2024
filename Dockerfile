FROM ubuntu:latest
RUN apt-get update && apt-get install -y picolisp readline-doc
COPY .inputrc /root
RUN mkdir /root/.pil
COPY pil/rc /root/.pil
COPY pil/viprc /root/.pil
WORKDIR /code
RUN /bin/bash
