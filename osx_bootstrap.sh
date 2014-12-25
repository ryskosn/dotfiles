#!/usr/bin/sh

# respect aereal
# https://github.com/aereal/dotfiles/blob/master/osx/defaults/config.bash

# Finder {{{
# show all ext
defaults write -g AppleShowAllExtensions -bool true
# search scope presant directory
defaults write com.apple.finder FXDefaultSearchScope -string 'SCcf'
# QuickLook what folder have
defaults write com.apple.finder QLEnableXRayFolders -bool true
# disable animation
defaults write com.apple.finder DisableAllAnimations -bool true
# show all file
defaults write com.apple.finder AppleShowAllFiles -bool true
# show status bar
defaults write com.apple.finder ShowStatusBar -bool true
# show path bar
defaults write com.apple.finder ShowPathbar -bool true
# list view
defaults write com.apple.finder FXPreferredViewStyle -string "Nslv"
# always expand save dialog
defaults write -g NSNavPanelExpandedStateForSaveMode -bool true
# download app show dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false
# warning ext change
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool true
# }}}

# Dock {{{
# auto hide
defaults write com.apple.dock autohide -bool true
# appear quicken
defaults write com.apple.dock autohide-delay -float 0
# minimize animation
defaults write com.apple.dock mineffect scale
# app minimize to icon
defaults write com.apple.dock minimize-to-application -bool true
# minimized app icon transparent
defaults write com.apple.dock showhidden -bool true
# 2D
defaults write com.apple.dock no-glass -bool true
# Big
defaults write com.apple.dock magnification -bool true
# hilite stack
defaults write com.apple.dock mouse-over-hilite-stack -bool true
# exose quicken
defaults write com.apple.dock expose-animation-duration -float 0.1
# dashboard not show as space
defaults write com.apple.dock dashboard-in-overlay -bool true
# dashboard disabled
defaults write com.apple.dashboard mcx-disabled -bool true
# }}}

# Screen Saver {{{
# require password imidiately
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0
# }}}

# Activity Monitor.app {{{
# Dock icon memory status
defaults write com.apple.ActivityMonitor IconType -int 4
# }}}

# App Store {{{
defaults write com.apple.appstore ShowDebugMenu -bool true
# }}}

# full keyboard access
defaults write -g AppleKeyboardUIMode -int 3
# menu bar not transparent
defaults write -g AppleEnableMenuBarTransparency -bool false
# scroll bar automatic
defaults write -g AppleShowScrollBars -string 'Automatic'
# Fn keys natural
defaults write -g com.apple.keyboard.fnState -bool true

# never keep wintows state
defaults write -g NSQuitAlwaysKeepsWindows -bool false
# pointer size maximum
# defaults write com.apple.universalaccess mouseDriverCursorSize -int 4

# Save to disk (not to iCloud)
defaults write -g NSDocumentSaveNewDocumentsToCloud -bool false

killall Dock
killall Finder
