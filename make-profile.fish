#!/usr/bin/fish

ls *.md \
| parallel ./make2.hs '{}' "{.}.html" \
  --css css/buttondown.css --css css/style.css

and pushd formulas/
and stack build --profile
popd

echo "Done."
