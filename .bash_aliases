alias cmd='xterm'

# Update all installed software:
alias pkg.update='sudo apt-get update; sudo apt-get upgrade'

# Search all available software:
alias pkg.search='apt-cache search'

# List all installed software:
alias pkg.list.installed='dpkg -l'

# List all available software:
alias pkg.list.available='dpkg -l \*'


# Files in this pkg
alias pkg.info='dpkg -S'

# Files in this pkg
alias pkg.files='dpkg -L'

# Install by registered NAME:
alias pkg.add='sudo apt-get install'

# Uninstall
alias pkg.del='sudo apt-get --purge remove'
