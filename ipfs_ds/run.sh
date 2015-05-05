#!/bin/bash

PREFIX="run.sh"

echo "${PREFIX}: Running cli module ... "
erl blocks.beam cli.beam -s cli cli -noshell

echo "${PREFIX}: [DONE]."
