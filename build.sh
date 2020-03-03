#!/usr/bin/env bash

set -eu
set -o pipefail

dotnet tool restore
dotnet build -c Release src