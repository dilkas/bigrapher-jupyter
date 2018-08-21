# BigraphER Jupyter

[![Jupyter protocol][protocol-img]][protocol] [![License][license-img]][license]

[license]:      https://github.com/akabe/ocaml-jupyter/blob/master/LICENSE
[license-img]:  https://img.shields.io/badge/license-MIT-blue.svg
[protocol]:     http://jupyter-client.readthedocs.io/en/stable/messaging.html
[protocol-img]: https://img.shields.io/badge/Jupyter%20protocol-5.2-blue.svg
[jupyter]:      http://jupyter.org/
[opam]:         https://opam.ocaml.org/

A BigraphER kernel for [Jupyter notebooks][jupyter] based on the [OCaml kernel](https://github.com/akabe/ocaml-jupyter).

## Getting started

### Manual installation

In order to use BigraphER Jupyter, you must first [install BigraphER](http://www.dcs.gla.ac.uk/~michele/bigrapher.html). Then run:

``` console
$ pip install jupyter
$ opam pin add jupyter https://github.com/dilkas/bigrapher-jupyter.git
$ opam pin add jupyter-archimedes https://github.com/dilkas/bigrapher-jupyter.git
# jupyter kernelspec install --name ocaml-jupyter "$(opam config var share)/jupyter"
```

To enable syntax highlighting, copy `big.js` over to `/usr/lib/python3.7/site-packages/notebook/static/components/codemirror/mode/big/` (create the last directory, if needed). Note: your Python version might be different from 3.7, and, depending on the version, `site-packages` might be called `dist-packages`. Similarly, to enable mouseover preview of bigraphs in state diagrams, copy `custom.js` over to `~/.jupyter/custom/`. Finally, in order to use the BigraphER API in OCaml code, two lines must be added to `~/.ocamlinit`:

```
#use "topfind";;
#require "bigraph";;
```

 You can use the kernel by launching Jupyter notebook server:

```console
$ jupyter notebook
```

### Docker image

Alternatively, one can download and run a Docker image. Once you have Docker installed, run

```console
# docker pull dilkas/bigrapher-jupyter
```

to download the image and

```console
# docker run -it -p 8888:8888 dilkas/bigrapher-jupyter
```

to start the Jupyter server. You can then use a web browser to visit `localhost:8888` and enter the password `big`. You will then be presented with an example notebook. In order to access directories on your file system, the image should be run as

```console
# docker run -it -p 8888:8888 -v local_path:/home/big/mounted dilkas/bigrapher-jupyter
```

Then the local directory `local_path` can be accessed as `~/mounted` through the Jupyter web interface. Note that `local_path` must be an absolute path.

## Usage

### Magics

The kernel supports several custom magics that can be used on the first line of a cell:

1. `%ocaml` magic allows you to run OCaml code.
2. `%output` makes the BigraphER's output visible (most of it is usually hidden).
3. `%clear` deletes the code from previously-run cells from the internal buffer.
4. `%states` produces a state diagram.
5. `%simulate n` runs a simulation for `n` steps, when used on non-stochastic models. For stochastic models, `n` is a floating-point number representing the maximum simulation time.

### Code completion

BigraphER Jupyter kernel supports [merlin](https://ocaml.github.io/merlin/)-based code completion for OCaml cells. Candidates are shown by Tab key like

![Code completion](https://akabe.github.io/ocaml-jupyter/images/completion.png)

The kernel uses [.merlin](https://github.com/ocaml/merlin/wiki/project-configuration) file at a notebook directory for completion.

### Inspection

_Inspection_ in Jupyter is also achieved by merlin. You can see documentation and type of an identifier by Shift+Tab key like

![Inspection](https://akabe.github.io/ocaml-jupyter/images/inspect.png)

### API documentation

Bigrapher Jupyter includes some sub-packages:

- [jupyter][jupyter-core] is a core library of OCaml Jupyter. This package is internally used. You don't need it directly.
- [jupyter.notebook][jupyter-notebook] is a library to control Jupyter from OCaml REPL in notebooks. This provides dynamic generation of HTML/markdown, and image embedding.
- [jupyter.comm][jupyter-comm] is a library for communication between OCaml notebooks and Jupyter/Web frontend.
- [jupyter-archimedes][jupyter-archimedes] is Jupyter backend of [Archimedes][archimedes], an easy-to-use 2D plotting library. This package only registers the `jupyter` backend to Archimedes, and provides empty interface.

[jupyter-core]:       https://akabe.github.io/ocaml-jupyter/api/jupyter/
[jupyter-notebook]:   https://akabe.github.io/ocaml-jupyter/api/jupyter/Jupyter_notebook/
[jupyter-comm]:       https://akabe.github.io/ocaml-jupyter/api/jupyter/Jupyter_comm/
[jupyter-archimedes]: https://akabe.github.io/ocaml-jupyter/api/jupyter-archimedes/
[archimedes]:         http://archimedes.forge.ocamlcore.org/

### Customize kernel parameters

A kernelspec JSON is saved at the following path:

```console
$ cat "$(opam config var share)/jupyter/kernel.json"
{
  "display_name": "OCaml 4.04.2",
  "language": "OCaml",
  "argv": [
    "/home/USERNAME/.opam/4.04.2/bin/ocaml-jupyter-kernel",
    "--init",
    "/home/USERNAME/.ocamlinit",
    "--verbosity",
    "info",
    "--connection-file",
    "{connection_file}"
  ]
}
```

See `ocaml-jupyter-kernel --help` for details of command-line parameters in `argv`. After you edit the file, re-register the kernel:

```console
$ jupyter kernelspec install --name ocaml-jupyter "$(opam config var share)/jupyter"
```