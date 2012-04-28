echo "Installs and packages YES (YES Erlang Scheduler)"
echo "Please make sure these are installed: git, erlang, gcc, make, autoconf"
echo -n "Press enter to continue..."
read

echo "Writing yaws.conf"
	curl -s http://memoricide.very.lv/yaws.conf | sed '/^\s*appmods/s/^/#/; 143 c\    docroot = '"$PWD"'/apps/yes_http' > yaws.conf
	mkdir logs

echo "Making fission"
	cd apps/fission
	make
	cd ../..
	mkdir data
