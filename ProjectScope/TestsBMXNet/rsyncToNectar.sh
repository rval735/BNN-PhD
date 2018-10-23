#!/bin/bash

cd ~/Dev/CNN-Phd/ProjectScope/TestsBMXNet
rsync -ar --update --delete --exclude="results/" --exclude="bnn/data/" . nectar:~/TestsBMXNet


# https://stackoverflow.com/questions/1515730/is-there-a-command-like-watch-or-inotifywait-on-the-mac

# You can use launchd for that purpose. Launchd can be configured to automatically launch a program when a file path is modified.
#
# For example the following launchd config plist will launch the program /usr/bin/logger when the desktop folder of my user account is modified:
#
# <?xml version="1.0" encoding="UTF-8"?>
# <!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
# <plist version="1.0">
# <dict>
#     <key>Label</key>
#     <string>logger</string>
#     <key>ProgramArguments</key>
#     <array>
#         <string>/usr/bin/logger</string>
#         <string>path modified</string>
#     </array>
#     <key>WatchPaths</key>
#     <array>
#         <string>/Users/sakra/Desktop/</string>
#     </array>
# </dict>
# </plist>
# To activate the config plist save it to the LaunchAgents folder in your Library folder as "logger.plist".
#
# From the shell you can then use the command launchctl to activate the logger.plist by running:
#
# $ launchctl load ~/Library/LaunchAgents/logger.plist
# The desktop folder is now being monitored. Every time it is changed you should see an output in the system.log (use Console.app). To deactivate the logger.plist, run:
#
# $ launchctl unload ~/Library/LaunchAgents/logger.plist
# The configuration file above uses the WatchPaths option. Alternatively you can also use the QueueDirectories option. See the launchd man page for more information.