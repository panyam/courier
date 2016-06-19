#!/bin/sh

rm -Rf build dist
python setup.py sdist
python setup.py bdist_wheel --universal
