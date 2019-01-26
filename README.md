# Jason Project and ticket Management

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Jason.svg)](http://jenkins.vacs.fr/job/Jason/)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/jason/1.0.0.svg)

Jason is a simple project and ticket management web application.
It is built on top of the Ada Web Application framework.

To build Jason you will need the following projects:

* AWA           (https://github.com/stcarrez/ada-awa)
* ASF           (https://github.com/stcarrez/ada-asf)
* ADO           (https://github.com/stcarrez/ada-ado)
* Ada Util      (https://github.com/stcarrez/ada-util)
* Ada Wiki      (https://github.com/stcarrez/ada-wiki)
* Ada EL        (https://github.com/stcarrez/ada-el)
* Ada Security  (https://github.com/stcarrez/ada-security)
* Dynamo        (https://github.com/stcarrez/dynamo)

Jason relies on the following external projects:

* AWS      (http://libre.adacore.com/libre/tools/aws/)
* XMLAda   (http://libre.adacore.com/libre/tools/xmlada/)

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

      