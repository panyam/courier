#!/usr/bin/env python

import os
import sys
from distutils.sysconfig import get_python_lib

from setuptools import find_packages, setup

# Warn if we are installing over top of an existing installation. 
overlay_warning = False
if "install" in sys.argv:
    lib_paths = [get_python_lib()]
    if lib_paths[0].startswith("/usr/lib/"):
        # We have to try also with an explicit prefix of /usr/local in order to
        # catch Debian's custom user site-packages directory.
        lib_paths.append(get_python_lib(prefix="/usr/local"))
    for lib_path in lib_paths:
        existing_path = os.path.abspath(os.path.join(lib_path, "courier"))
        if os.path.exists(existing_path):
            # We note the need for the warning here, but present it after the
            # command is run, so it's more likely to be seen.
            overlay_warning = True
            break


# Any packages inside the Courier source folder we dont want in the packages
EXCLUDE_FROM_PACKAGES = [ ]

# Dynamically calculate the version based on courier.VERSION.
version = __import__('courier').get_version()

setup(
    name='courierlib',
    version=version,
    url='https://github.com/panyam/courier',
    author='Sriram Panyam',
    author_email='sri.panyam@gmail.com',
    description=("Modern data interchange system for web + mobile stacks "
                 "that combines an expressive schema language with language "
                 "idiomatic data binding generators. "),
    license='BSD',
    packages=find_packages(exclude=EXCLUDE_FROM_PACKAGES),
    include_package_data=True,
    scripts=['bin/clean_install.sh', 'bin/package.sh'],
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        'Topic :: Internet :: WWW/HTTP',
        'Topic :: Internet :: WWW/HTTP :: Dynamic Content',
        'Topic :: Internet :: WWW/HTTP :: WSGI',
        'Topic :: Software Development :: Libraries :: Application Frameworks',
        'Topic :: Software Development :: Libraries :: Python Modules',
    ],
)


if overlay_warning:
    sys.stderr.write("""

========
WARNING!
========

You have just installed Courier over top of an existing
installation, without removing it first. Because of this,
your install may now include extraneous files from a
previous version that have since been removed from
Courier. This is known to cause a variety of problems. You
should manually remove the

%(existing_path)s

directory and re-install Courier.

""" % {"existing_path": existing_path})
