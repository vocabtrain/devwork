#!/bin/zsh
env='Development'
if [[ `hostname` == 'eos' ]]; then
	absdir=/home/niki/eos
else
	absdir=/home/niki/
fi
datadir=$absdir/data/
logdir=$absdir/data/log
scriptdir=$absdir/devwork/cron
schemedir=$absdir/devwork/schemes
enginedir=$absdir/devwork/engine
postgresconfig=$enginedir/config/postgresql.yml

