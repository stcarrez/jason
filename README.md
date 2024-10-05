# Jason Project and ticket Management

[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/jason/badges/build.json)](https://porion.vacs.fr/porion/projects/view/jason/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/jason/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/jason/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/jason/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/jason/summary)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)

Jason is a simple project and ticket management web application.
Jason comes as a plugin for the [Ada Web Application](https://github.com/stcarrez/ada-awa) so that
it can easily be integrated in other project based on AWA.

# Building Jason

The project uses [Alire](https://github.com/alire-project/alire) to build and manage dependencies.
To use Jason you your AWA project, you can add the `jason` crate dependency as follows:

```
alr with jason
```

Before building and configuring Jason, you should have configured, built and installed
all of the above projects.

# Building Jason

To configure Jason, use the following command:
```
   ./configure
```
Then, build the application:
```
   make generate build
```

# Running Jason

You will then start the application as follows:
```
   bin/jason-server
```

and point your browser to http://localhost:8080/jason/index.html
On your browser, you will first enter in the setup phase to configure
the database and the application.

      