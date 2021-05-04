#!/usr/bin/env bash

interpreter=src/TestWiadrexLang
examples_path=lang/examples

echo -e "================================================================="
echo -e "Running correct examples. All should execute properly"

for example in $examples_path/good/*.wdr; do
  echo -e "-----------------------------------------------------------------"
  echo -e "Running $example:\n"
  echo -e "-----"
  cat "$example" | ./$interpreter
done

echo -e "================================================================="
echo -e "Running incorrect examples. All should fail"

for example in $examples_path/bad/*.wdr; do
  echo -e "-----------------------------------------------------------------"
  echo -e "Running $example:\n"
  echo -e "-----"
  cat "$example" | ./$interpreter
done