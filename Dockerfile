FROM mseve/bigrapher

ENV OPAMBUILDDOC 0

RUN sudo apt-get -qy update && \
    DEBIAN_FRONTEND=noninteractive sudo apt-get -qy upgrade && \
    DEBIAN_FRONTEND=noninteractive sudo apt-get -qy --no-install-recommends install \
                                                python-pip \
                                                libgmp3-dev \
                                                libzmq3-dev \
                                                libcairo2-dev && \
    sudo apt-get clean && \
    sudo -H pip install setuptools wheel && \
    sudo -H pip install jupyter && \
    opam update && \
    opam upgrade && \
    opam pin add jupyter https://github.com/dilkas/bigrapher-jupyter.git && \
    opam pin add jupyter-archimedes https://github.com/dilkas/bigrapher-jupyter.git && \
    sudo jupyter kernelspec install --name ocaml-jupyter "$(opam config var share)/jupyter" && \
    sudo mkdir /usr/local/lib/python2.7/dist-packages/notebook/static/components/codemirror/mode/big && \
    echo "#use \"topfind\";;\n#require \"bigraph\";;" >> .ocamlinit && \
    jupyter notebook --generate-config && \
    echo "c.NotebookApp.password='sha1:2245daae8ac5:bc9ea5901150143280d789e071c7b64b31a3a7c8'" \
    >> .jupyter/jupyter_notebook_config.py && \
    mkdir .jupyter/custom

ADD big.js /usr/local/lib/python2.7/dist-packages/notebook/static/components/codemirror/mode/big/
ADD custom.js .jupyter/custom/
ADD notebooks/Example_Notebook.ipynb ./
ADD startup.sh ./

RUN sudo chown -R big:big .local $HOME/Example_Notebook.ipynb startup.sh .jupyter && \
    chmod +x startup.sh

ENTRYPOINT [ "./startup.sh" ]