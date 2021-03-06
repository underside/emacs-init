help:  ## This is help dialog. Enter "make" to get help
	@echo ''
	@echo '	 packages            Install general packages that usually needed in freshly installed Linux'
	@echo '	 golang_packages     Install packages golang development'
	@echo '	 golang_env          Set ENV vars for golang dev'
	@echo '	 git_env             Setup git env settings: git username and email '
	@echo '	 general_conf        Setup general settings: yamllint, '
	@echo ''

packages:
	-sudo apt install -y git
	-sudo apt install -y fonts-hack
	-sudo apt install -y emacs

golang_packages:
	-wget https://golang.org/dl/go1.15.6.linux-amd64.tar.gz
	-sudo tar -C /usr/local -xzf go1.15.6.linux-amd64.tar.gz
	-sudo apt install golang-mode
	-go get -u github.com/nsf/gocode
	-go get -u github.com/rogpeppe/godef
	-go get -u github.com/jstemmer/gotags
	-go get -u github.com/kisielk/errcheck
	-go get -u golang.org/x/tools/cmd/guru
	-go get -u github.com/golang/lint/golint
	-go get -u golang.org/x/tools/cmd/gorename
	-go get -u golang.org/x/tools/cmd/goimports
	-sudo go get -u golang.org/x/tools/cmd/godoc

golang_env_linux:
	-echo 'export PATH=$$PATH:/usr/local/go/bin' >> ~/.bashrc
	-echo 'export GOPATH=$$PATH:/home/y/go' >> ~/.bashrc
	-echo 'export GOROOT=$$PATH:/usr/local/go' >> ~/.bashrc
	-echo 'export PATH=$$PATH:$$GOROOT/bin' >> ~/.bashrc
	-echo 'export PATH=$$PATH:$$GOPATH/bin' >> ~/.bashrc
	-source ~/.bashrc

git_env:
	-git config --global user.name ""
	-git config --global user.email ""
	-git config --global credential.helper store

general_conf:
	-mkdir -p ~/.config/yamllint
	-cp ./conf/yamllint_config ~/.config/yamllint/config.yaml


