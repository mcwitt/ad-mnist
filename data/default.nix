with import <nixpkgs> { };

let

  train-images = fetchurl {
    url = "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz";
    sha256 = "029na81z5a1c9l1a8472dgshami6f2iixs3m2ji6ym6cffzwl3s4";
  };

  train-labels = fetchurl {
    url = "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz";
    sha256 = "0p152200wwx0w65sqb65grb3v8ncjp230aykmvbbx2sm19556lim";
  };

  test-images = fetchurl {
    url = "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz";
    sha256 = "1rn4vfigaxn2ms24bf4jwzzflgp3hvz0gksvb8j7j70w19xjqhld";
  };

  test-labels = fetchurl {
    url = "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz";
    sha256 = "1imf0i194ndjxzxdx87zlgn728xx3p1qhq1ssbmnvv005vwn1bpp";
  };

in runCommand "mnist-data" { } ''
  mkdir -p $out
  ${gzip}/bin/zcat ${train-images} > $out/train-images && \
  ${gzip}/bin/zcat ${train-labels} > $out/train-labels && \
  ${gzip}/bin/zcat ${test-images} > $out/test-images && \
  ${gzip}/bin/zcat ${test-labels} > $out/test-labels
''
