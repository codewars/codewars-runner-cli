echo "make sure you run this with bash"
echo -n $((( RANDOM % 60 ))) > cronfile
echo " * * * * service docker restart" >> cronfile
crontab cronfile
rm cronfile
