reflex-material-bootstrap
=========================

This is factored out library of generic frontend widgets that are used in HexResearch projects. The library is built around
[reflex-dom](https://github.com/reflex-frp/reflex-dom) and [Bootstrap Material](http://fezvrasta.github.io/bootstrap-material-design/bootstrap-elements.html).

GHCJS configuration
===================

Checked configuration for stack:
``` yaml
resolver: lts-8.11
compiler: ghcjs-0.2.1.9008011_ghc-8.0.2
compiler-check: match-exact
setup-info:
  ghcjs:
    source:
      ghcjs-0.2.1.9008011_ghc-8.0.2:
        url: https://github.com/hexresearch/ghcjs-stack-dist/raw/master/ghcjs-0.2.1.9008011.tar.gz
        sha1: a72a5181124baf64bcd0e68a8726e65914473b3b

packages:
- '.'
- '../../reflex-material-bootstrap'
- '../reports-manager-api'
- location:
    git: https://github.com/NCrashed/ghcjs-base
    commit: 89c2d2fd9683fe31efac585c58d1cd909a65ba88
  extra-dep: true
- location:
    git: git@github.com:reflex-frp/reflex.git
    commit: 50305c797c41a27660b74f543e204e902c086bbf
  extra-dep: true
- location:
    git: git@github.com:reflex-frp/reflex-dom.git
    commit: 2bc47782128a9c71222816afca407997d47f043a
  extra-dep: true
  subdirs:
    - reflex-dom
    - reflex-dom-core
- location:
    git: git@github.com:ghcjs/ghcjs-ffiqq.git
    commit: b52338c2dcd3b0707bc8aff2e171411614d4aedb
  extra-dep: true

extra-deps:
- aeson-1.2.1.0
- cabal-doctest-1.0.2
- dependent-sum-template-0.0.0.6
- ghcjs-dom-0.8.0.0
- ghcjs-dom-jsffi-0.8.0.0
- jsaddle-0.8.3.2
- prim-uniq-0.1.0.1
- ref-tf-0.4.0.1
- zenc-0.1.1
```

* Tip: use `ghc-options: -dedupe -O2` to reduce size of resulted JS blob.
* Tip: for production use Google Closure compiler with settings:
``` bash
cp $(stack path --local-install-root)/bin/<packagename>.jsexe/{all.js,all.js.externs} .
cp ../aol-invoice-server/static/jquery-1.9.1.js .
cp ../aol-invoice-server/static/bootstrap/js/bootstrap.min.js .
cp ../aol-invoice-server/static/bootstrap/js/material.min.js .
cp ../aol-invoice-server/static/bootstrap/js/ripples.min.js .
ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS \
  --externs=node \
  --externs=all.js.externs \
  --externs=bootstrap.min.js \
  --externs=material.min.js \
  --externs=ripples.min.js \
  --jscomp_off=duplicate \
  --jscomp_off=undefinedVars \
  > all.min.js
```
This tears down 12 MB JS bundle to 1 MB and then you can transfer it with enabled gzip on server.
That leads to 600 KB transferred to customer instead of original 12 MB.

Index page that is checked to be operational:
``` HTML
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta http-equiv="Content-Type" content="text/html;charset=ISO-8859-1">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
    <title>My page</title>

    <!-- Bootstrap -->
    <link href="bootstrap/css/bootstrap.min.css" rel="stylesheet">
    <link href="bootstrap/css/bootstrap-material-design.min.css" rel="stylesheet">
    <link href="bootstrap/css/ripples.min.css" rel="stylesheet">
    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
    <link href="style.css" rel="stylesheet">

    <!-- Compiled app -->
    <script language="javascript" src="all.js"></script>
  </head>
  <body>
  </body>
  <script language="javascript" src="jquery-1.9.1.js"></script>
  <script language="javascript" src="bootstrap/js/bootstrap.min.js"></script>
  <script language="javascript" src="bootstrap/js/material.min.js"></script>
  <script language="javascript" src="bootstrap/js/ripples.min.js"></script>
  <script language="javascript" src="runmain.js" defer></script>
  <script>
    $(document).ready(function(){
        $.material.init();
    });
  </script>
</html>
```
