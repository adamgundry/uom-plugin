#!/bin/sh

#set +v

stack install build-uom-plugin
build-uom-plugin $@
