java -Xmx512M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -jar `dirname $0`/sbt-launch-0.7.4.jar "$@"
