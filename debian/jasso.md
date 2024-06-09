% jasso(1) | User Commands
%
% "June 7 2024"

# NAME

jasso - Jasso Single Sign-On System from Piglet Engineering

# SYNOPSIS

**jasso** _file_

**jasso** [{**-h** | *\-\-help**} | {**-v** | **\-\-version**}]

# DESCRIPTION

*jasso* is a modular identity provider (IdP).  It currently supports:
           Source   Sink
  LDAP     yes      no
  SAML2.0  no       yes
  SCIM     no       no
  Radius   no       no
  OAUTH2   no       no
  Postgres no       -

# OPTIONS

_file_ overrides the location of the configuration file.

**-h**, **\-\-help**
:   Show summary of options.

**-v**, **\-\-version**
:   Show version of program.

# ENVIRONMENT

**JASSOCONF**
:   If set, and no value is specified on the command line, its value is used as the path of the configuration file (see also the section called “FILES”).

# FILES

/etc/jasso.conf
:   The system-wide configuration file to control the behaviour of jasso.  Can be overridden by the **JASSOCONF** environment variable or on the command line.  See **jasso.conf**(5) for further details.

# BUGS

Please report to _Piglet Engineering Support <support@piglet.ch>_.

# SEE ALSO

**jasso.conf**(5)

# AUTHOR

Pete Ryland <pdr@piglet.ch>

# COPYRIGHT

Copyright © 2021-2024 Homebrew Holdings Pty Ltd
