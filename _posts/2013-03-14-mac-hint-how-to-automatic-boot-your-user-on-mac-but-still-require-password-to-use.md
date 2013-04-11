---
layout: post
title: Mac hint, how to automatic boot your user on mac, but still require password to use!
published: true
---
Hi folks!

Just started this new blog, finally got myself wanting to blog a bit, so, let's go!

> I did a text tutorial full covering the process, and just after finish writing I decided that recording a video tutorial would be better, but I spent time with the text, so I'll leave it after the video anyway.

<div style="margin: 0 auto; width: 853px">
  <iframe width="853" height="480" src="http://www.youtube.com/embed/8iuIHjvh9Xg" frameborder="0" allowfullscreen></iframe>
</div>

---

Following text is how I figure out, if you don't care and just want the solution, quickly look though until you find SOLUTION HERE!!

Well, like most of you, I'm not confortable leaving my mac auto login without requering any password, but, one think that I really like about automatic login is that you system never stops until it's loads everything you need to use it! By having to login, you waste that time between your basic system loads and you write your credentials... which sux...

So, I was thinking about how can I get the best of both, how to make the Mac loads everything (system + my user stuff), but without losing my securty(in order words, I still want to require the password!)?

So, I had one idea, that's actually pretty simple :D

As you know, you can "force password prompt" by simply running the `ScreenSaver` (just make sure that you require credentials at screen lock, it's at `Preferences -> Security & Privacy`)! 

### SOLUTION HERE!!

Hello, you don't just jumped here right? I took a while writing the motivation and the process, would be a kinda rude if you skipped the lovely text that I wrote for you... But I'm sure you didn't, right? :)

Anyway, steps in order to configure your "non-stop secure automatic login":

1. Make sure your `Security Preferences` are set to require login immediatly after Screen Saver, like on this screenshot: ![Security Preferences](https://dl.dropbox.com/u/1772210/Screen%20Shot%202013-03-15%20at%203.53.47%20AM.png)
2. Go on `Preferences -> Users & Groups -> Login Options` and setup your automatic login
3. Click on your user on same page, and go on `Login Items` tab
4. Add `ScreenSaverEngine` app, you will find it at `/System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources`

And that's it, hope you enjoyed the tutorial, have fun!
