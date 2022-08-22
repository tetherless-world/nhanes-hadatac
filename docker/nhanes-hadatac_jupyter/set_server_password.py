import json
from jupyter_server.auth import passwd

hashed_password = passwd("hadatac") # Substitute 'hadatac' with your password
server_config = {
  "ServerApp": {
    "password": hashed_password
  }
}

with open(".jupyter/jupyter_server_config.json", "w") as outfile:
    json.dump(server_config, outfile)