#!/bin/sh

eval $(opam config env);
jupyter notebook --ip 0.0.0.0 --no-browser --allow-root
