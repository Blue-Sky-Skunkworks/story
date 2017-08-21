## Story

### Install Notes
Make sure libmagic and sassc are brew installed or apt-get/emerge'd, etc first
Also, you probably want the latest sbcl/quicklisp
  * and make sure you (ql:update-client) and (ql:update-all-dists)
sudo mkdir -p /mnt/projects/cores && sudo chown -R $(whoami):wheel /mnt/projects
  * Since this is will-ware and various scripts rely on this directory for image storage
Symlink/clone story and cl-ascii-art into local-projects
Now, go ahead and quickload all the necessary code in sbcl: (ql:quickload '(story story-modules))
Perfect, now you can run ./start to build a core and get hacking.
Swank will be serving on port 4005, and the app will be available on 3300. User guide coming soon...

### User Manual

Coming soon!
