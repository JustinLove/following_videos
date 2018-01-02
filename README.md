# Following Videos

Twich removed the list of videos by people you follow. This create s simple version of it.

## Usage

Built using [Elm](http://elm-lang.org/)

A [Twitch Client-ID](https://dev.twitch.tv/docs/authentication#registration) is required to make API calls. This is defined in `src/TwtichId.elm`. This file is not part of of the repo, but `src/Twitch/Id.elm.example` can be copied and edited to provide your client id.

My build command:

> `elm-make src/FollowingVideos.elm --output public/following_videos.js`

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

The generated page is styled after the twitch following list.

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
