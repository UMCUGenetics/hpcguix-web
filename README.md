hpcguix-web
===========

Hello!  hpcguix-web is the [GNU Guix](https://guix.gnu.org) package
browser that runs [on the Guix-HPC web
site](https://hpc.guix.info/browse) and at
[hpcguix.op.umcutrecht.nl](https://hpcguix.op.umcutrecht.nl).  It
provides a handy package browser that requires JavaScript.  The list of
packages that it displays is automatically updated periodically.  You
can customize it, for instance by specifying the list of *channels* to
use as the source of packages, or by changing the appearance of package
pages.

To run it yourself, you need to install Guix and enter an isolated
development shell (a container) with:

```
guix shell -CPNW
```

From that shell, run the following commands:

```
# Build the code.
autoreconf -vfi
./configure
make

# Run the web interface
./env ./hpcguix-web
```

Guix System also [provides a
service](https://guix.gnu.org/manual/en/html_node/Web-Services.html#index-hpcguix_002dweb_002dservice_002dtype)
that allows you to set it up by adding a single line to the `services`
field of your operating system declaration.

Please send bug reports and suggestions to `bug-guix@gnu.org`.  Enjoy!
