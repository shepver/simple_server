#!/bin/bash

erl -sname simple_server -pa ./ebin deps/*/ebin -s simple_server start
