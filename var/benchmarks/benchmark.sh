#!/usr/bin/env bash

## Get script directory:
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

## Get the data directory:
DIR_DATA="${DIR}/data/"

## Get the results directory:
DIR_RSLT="${DIR}/results/"

## Output file:
FILE="${DIR_RSLT}/result_$(date +"%Y-%m-%sT%H:%M:%S".csv)"

## Function to run a command and measure its resident memory and runtime.
run () {
    /usr/bin/time --format "%M,%e" "${1}" spec.yaml "${2}" > /dev/null
}

## List example input sets:
echo "file,size,memory,time" | tee "${FILE}"
find "${DIR_DATA}" -name "*.csv" | sort | while read -r _line; do
    ## Get the size:
    _size="$(tail -n +2 "${_line}" | wc -l)"
    for _ in $(seq 4); do
	_out="$(run "${1:-"habulara"}" "${_line}" 2>&1 | tail -n +2)"
	echo "${_line},${_size},${_out}" | tee -a "${FILE}"
    done
done

echo "Output is saved to: ${FILE}"
