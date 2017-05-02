#
# ROS bash setup 
# (note: Python 2.7 required for some older packages)
#

jade() {
  source /opt/ros/jade/setup.bash
  export PYTHONPATH=/opt/ros/jade/lib/python2.7/site-packages:$PYTHONPATH
  export PKG_CONFIG_PATH="/opt/ros/jade/lib/pkgconfig:$PKG_CONFIG_PATH"

  # If you wind up with a standard package path:
  # export ROS_PACKAGE_PATH=/path/to/your/package/path:$ROS_PACKAGE_PATH

  alias catkin_make="catkin_make -DPYTHON_EXECUTABLE=/usr/bin/python2 -DPYTHON_INCLUDE_DIR=/usr/include/python2.7 -DPYTHON_LIBRARY=/usr/lib/libpython2.7.so"

  source /usr/share/gazebo/setup.sh
}

indigo() {
  source /opt/ros/indigo/setup.bash
  export PYTHONPATH=/usr/lib/python2.7/site-packages:/opt/ros/indigo/lib/python2.7/site-packages:$PYTHONPATH
  export PKG_CONFIG_PATH="/opt/ros/indigo/lib/pkgconfig:$PKG_CONFIG_PATH"

  # If you wind up with a standard package path:
  # export ROS_PACKAGE_PATH=/path/to/your/package/path:$ROS_PACKAGE_PATH

  alias catkin_make="catkin_make -DPYTHON_EXECUTABLE=/usr/bin/python2 -DPYTHON_INCLUDE_DIR=/usr/include/python2.7 -DPYTHON_LIBRARY=/usr/lib/libpython2.7.so"

  source /usr/share/gazebo/setup.sh
}
