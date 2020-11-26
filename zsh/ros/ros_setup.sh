####################################################
# ROS shell setup
#  (and why humanity will never escape python 2)
#
####################################################

melodic() {
  setup="/opt/ros/melodic/setup.zsh"
  if [ ! -d "$setup" ]; then
      echo "shell config not found at $setup"
      return 1
  fi
  source $setup
}

lunar() {
  setup="/opt/ros/lunar/setup.zsh"
  if [ ! -d "$setup" ]; then                                                                             
      echo "shell config not found at $setup"                                                            
      return 1                                                                                           
  fi
  source $setup

  export PYTHONPATH=/opt/ros/lunar/lib/python2.7/site-packages:$PYTHONPATH
  export PKG_CONFIG_PATH="/opt/ros/lunar/lib/pkgconfig:$PKG_CONFIG_PATH"

  alias catkin_make="catkin_make -DPYTHON_EXECUTABLE=/usr/bin/python2 -DPYTHON_INCLUDE_DIR=/usr/include/python2.7 -DPYTHON_LIBRARY=/usr/lib/libpython2.7.so"
}

