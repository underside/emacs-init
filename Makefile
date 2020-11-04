help:  ## This is help dialog. Enter "make" to get help
	@echo ''
	@echo '	 install_packages     Install packages that usually needed in freshly installed Linux'
	@echo '	 setup_env            Setup env settings: git username and email '
	@echo ''

install_packages:
	-sudo apt install -y git
	-sudo apt install -y fonts-hack
setup_env:
	-git config --global user.name ""
	-git config --global user.email ""
	-git config --global credential.helper store
