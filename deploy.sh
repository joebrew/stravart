#!/bin/bash
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" /home/joebrew/Documents/stravart/app.R   ubuntu@bohemia.team:/home/ubuntu/Documents/stravart/app.R
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" /home/joebrew/Documents/stravart/utils.R   ubuntu@bohemia.team:/home/ubuntu/Documents/stravart/utils.R
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" /home/joebrew/Documents/stravart/global.R   ubuntu@bohemia.team:/home/ubuntu/Documents/stravart/global.R
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" /home/joebrew/Documents/stravart/credentials.yaml  ubuntu@bohemia.team:/home/ubuntu/Documents/stravart/credentials.yaml
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" /home/joebrew/Documents/stravart/plot_functions.R  ubuntu@bohemia.team:/home/ubuntu/Documents/stravart/plot_functions.R
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" /home/joebrew/Documents/stravart/cache  ubuntu@bohemia.team:/home/ubuntu/Documents/stravart/cache

: '
Run the below on the server side:

cd /srv/shiny-server/stravart;
cp ~/Documents/stravart/app.R .;
cp ~/Documents/stravart/utils.R .;
cp ~/Documents/stravart/global.R .;
cp ~/Documents/stravart/credentials.yaml .;
cp ~/Documents/stravart/plot_functions.R .;
cp -R ~/Documents/stravart/cache .;

sudo chmod -R 775 /srv/shiny-server/stravart
sudo chown -R shiny /srv/shiny-server/stravart

sudo systemctl restart shiny-server;

'

: '
# Update the cache files
scp -r -i "/home/joebrew/.ssh/openhdskey.pem"   ubuntu@bohemia.team:/srv/shiny-server/stravart/cache /home/joebrew/Documents/stravart/
'
