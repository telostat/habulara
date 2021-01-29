#!/usr/bin/env bash

## Get script directory:
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

## Benchmarks data directory:
BENCHDIR="${DIR}/benchmarks/"

## Output file:
FILE="${BENCHDIR}/output_$(date +"%Y-%m-%sT%H:%M:%S".csv)"

## Ensure that the benchmark directory exists:
mkdir -p "${BENCHDIR}"

run () {
    /usr/bin/time --format "%M,%e" "${1}" "${2}" > /dev/null
}

## List example input sets:
echo "file,size,memory,time" | tee "${FILE}"
find "${DIR}" -name "generated_*.csv" | sort | while read -r _line; do
    ## Get the size:
    _size="$(tail -n +2 "${_line}" | wc -l)"
    for _ in $(seq 10); do
	_out="$(run "${1}" "${_line}" 2>&1 | tail -n +2)"
	echo "${_line},${_size},${_out}" | tee -a "${FILE}"
    done
done

echo "Output is saved to: ${FILE}"
