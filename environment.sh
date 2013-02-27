#!/bin/zsh
env='Development'
if [[ `hostname` == 'eos' ]]; then
	absdir=/home/niki/eos
else
	absdir=/home/niki/
fi
absdir=/home/niki
backupdir=$absdir/backup/db
datadir=$absdir/data
logdir=$absdir/data/log
scriptdir=$absdir/devwork/cron
schemedir=$absdir/devwork/schemes
enginedir=$absdir/devwork/engine
postgresconfig=$enginedir/config/postgresql.yml

