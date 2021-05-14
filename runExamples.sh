#!/usr/bin/env bash

interpreter=interpreter
examples_path=lang/examples

echo -e "================================================================="
echo -e "Running correct examples. All should execute properly"

all_good=0
passed_good=0
for example in $examples_path/good/*.wdr; do
  echo -e "-----------------------------------------------------------------"
  echo -e "Running $example:\n"
  echo -e "------------------------------"
  ./$interpreter "$example"

  if [[ $? -eq 0 ]]; then
    ((passed_good=passed_good+1))
    echo -e "\n\n-- OK --\n"
  else
    echo -e "\n\n-- WA --\n"
  fi
  ((all_good=all_good+1))
done

echo -e "================================================================="
echo -e "Running incorrect examples. All should fail"

all_bad=0
passed_bad=0
for example in $examples_path/bad/*.wdr; do
  echo -e "-----------------------------------------------------------------"
  echo -e "Running $example:\n"
  echo -e "------------------------------"
  ./$interpreter "$example"

  if [[ $? -eq 1 ]]; then
    ((passed_bad=passed_bad+1))
    echo -e "\n\n-- OK --\n"
  else
    echo -e "\n\n-- WA --\n"
  fi
  ((all_bad=all_bad+1))
done

good_percentage=$((passed_good * 100 / all_good))
bad_percentage=$((passed_bad * 100 / all_bad))
all_passed=$((passed_good + passed_bad))
all=$((all_good + all_bad))
all_percentage=$(((passed_good + passed_bad) * 100 / (all_good + all_bad)))
echo -e "================================================================="
echo -e "Good passed: $passed_good / $all_good ($good_percentage %)"
echo -e "Bad passed:  $passed_bad / $all_bad ($bad_percentage %)"
echo -e "-----------------------------------------------------------------"
echo -e "All passed:  $all_passed / $all ($all_percentage %)"