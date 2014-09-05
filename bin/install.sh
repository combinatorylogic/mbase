#!/bin/sh

ls MBase*.dll | xargs -I {} gacutil /i {}

