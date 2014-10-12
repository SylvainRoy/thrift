cd ~/dev/repositories/thrift/compiler/cpp
make -k

cd ~/dev/repositories/thrift/tutorial
rm -rf el/generated-el/*
~/dev/repositories/thrift/compiler/cpp/thrift -out ./el/generated-el --gen el tutorial.thrift
~/dev/repositories/thrift/compiler/cpp/thrift -out ./el/generated-el --gen el shared.thrift
