#!/bin/sh

if [ -z "$SED" ];
then
  SED=sed
fi

gacutil /l | grep d53ce51f54584d25 | $SED -s "s/ //g" | $SED -s "s/,Custom=null//g" | xargs -I {} gacutil /uf "{}"

