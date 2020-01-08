#!/bin/bash
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" /home/joebrew/Documents/stravart/   ubuntu@bohemia.team:/home/ubuntu/Documents/stravart/

: '
Run the below on the server side:

cd /srv/shiny-server/;
sudo rm -r stravart;
sudo cp -r ~/Documents/stravart/ .;
sudo chmod -R 775 stravart
sudo chown -R shiny stravart
sudo systemctl restart shiny-server;

'
