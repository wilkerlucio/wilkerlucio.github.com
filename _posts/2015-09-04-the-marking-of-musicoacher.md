---
layout: post
title: The Making of Musicoacher
published: true
category: "Clojure"
tags: ['Clojure', 'ClojureScript', 'Om', 'Figwheel', 'Devcards', 'Musicoacher']
---
# The Making of Musicoacher

[Musicoacher](http://www.musicoacher.com) is a free music teaching tool that I just released this week, and as
[Bruce Hauman said](https://twitter.com/bhauman/status/639064245964029952) "now the
really hard part of writing a blog post about it".

Here I'll describe some of the things that were used to build Musicoacher, this is not
to be a complete thing, just the parts that I think are more interesting to share with
you.

So let's go for it!

## The Stack

First at higher level, Musicoacher is written mostly on [ClojureScript](https://github.com/clojure/clojurescript)
that is my favorite language at this time, if you don't know it, [you should](https://www.youtube.com/watch?v=MTawgp3SKy8). Going down the stack
we have [React.JS](http://facebook.github.io/react/) being the bigger framework behind
the scenes, and I'm using it though [Om](https://github.com/omcljs/om) which is a wrapper
for React.JS with some goodies that makes sense on ClojureScript context, and also other
ClojureScript libraries that I'll mention later.

There is no custom server for this app, dealing with server configuration is something
that I like to avoid as much as possible, and having the idea that at some point I would
be smashing my head on the wall because I can't figure out how to scale my service was
a pretty hard reason for me to decide to delegate that part to someone else, so I can
focus on the parts that I can do better.

There are a bunch of options these days if you don't want to write your own server, for
Musicoacher [Parse.com](http://www.parse.com) is being used, I evaluated it against
[Firebase](http://www.firebase.com) and maybe using [JAWS](https://github.com/jaws-stack/JAWS),
but for the sake of easines Parse had the better feature case for my needs, which on this
case included `Database Storage`, `User Management` and `ACLs`,
also a good `Analytics` built in was helpful as well.

There are some lines of pure Javascript on the Cloud Code, I would like to convert this code
into ClojureScript later, maybe I'll try even to run as bootstrapped CLJS there, would be
great, but for now it's just plain Javascript there.

## Environment

Starting by the editor, the choice here goes to [IntelliJ Idea](https://www.jetbrains.com/idea/) + [Cursive](https://cursiveclojure.com),
Cursive provides everything that you would expected from and IDE to have with Clojure,
it's the easiest Clojure enviroment to setup that I've tried, I highly recommend it.

The next important piece on the enviroment is [Figwheel](https://github.com/bhauman/lein-figwheel),
it provides a very interactive development loop, I just save a file and all my browsers
are instantly updated, this is not just a plain reload like people do with live-reload
because the code is updated while the app state is maintened, if you wanna know more
I recommend you to watch [Bruce Hauman presentation](https://www.youtube.com/watch?v=j-kj2qwJa_E)
on it, you will be amazed.

Now follow me with this "component development simulation", see if you find yourself
familiar with at least some of these steps:

You have a blank page, then you start writing a component (let's use the chord rendered
that we use on the Chord Book on Musicoacher), we start with the simplest version, it just
renders it as Silent (no fingers placed anywhere), we write the code, we view the component
and it looks great! ST1*

Now the simple case is ready, we need make this component to support a new state that
includes some fingers on the fretboard, then we update the state of the component and
write that new code to support one example with that, and now we see it rendered, great
again! ST2*

If you do as I do, the problem now is that we have a view of the component on ST2, but
we just got rid of your example of ST1, maybe you don't do it at this time, maybe you
keep both, but usually, eventually you will get rid of those examples to make the real
use cases, and our simplest representations that we used to develop our component are lost...

But why we do that? Why don't we keep those representations so we could in future just
see if they still working? Wouldn't be nice to have a part of our app dedicated to those
kind of simple examples, where you could develop then, and go back to check on then anytime,
without having to manually put your app into an specific state to test. And that's
what [Devcards](https://github.com/bhauman/devcards) is about! Devcards provides an
enviroment for you to write those small UI's and components and keep it there to check
whenever you want, it's your app personal development playground, where you can write new
stuff anytime as isolated, and can go back to check anytime on the same way, and it supports
tests too.

Here I leave a few screenshots of how I'm using devcards so it may clarifiy some ideas for
you:

Devcards main, listing card namespaces

![Devcards Namespaces]({{ site.url }}/assets/the-making-of-musicoacher/devcards-namespaces.png)

Cards for examples on the chord vizualization

![Devcards Chords]({{ site.url }}/assets/the-making-of-musicoacher/devcards-chords.png)

This card allows me to visually define chords then just copy the chord data

![Devcards Builder]({{ site.url }}/assets/the-making-of-musicoacher/devcards-builder.png)

A few tests running in devcards

![Devcards Tests]({{ site.url }}/assets/the-making-of-musicoacher/devcards-test.png)

## Recognizing chords

A very fun part to work here was to write the code for the music theory, here we are
going to see the process that happens when you create a chord using the chord editor.

The chord editor

![Chord Editor]({{ site.url }}/assets/the-making-of-musicoacher/chord-editor.png)

There is a more basic fretboard component that does just the fretboard rendering and
trigger events when notes are clicked over there, then the `chord-editor` component
uses it an implements clicking to generate the chord, the process consists as:

```clojure
{:on-click-note (fn [[string fret]]
                 (let [value       (om/get-props owner :value)
                       tunning     (om/get-props owner :tunning)
                       new-strings (if (= (get-in value [:chord/strings string]) fret)
                                     (dissoc (:chord/strings value) string)
                                     (assoc (:chord/strings value) string fret))]

                   (u/call-state-fn owner :on-change {:db/id         (get value :db/id)
                                                      :chord/strings new-strings
                                                      :chord/name    (n/name-chord (n/strings->notes tunning new-strings))
                                                      :chord/fingers (n/string-fingers new-strings)})))}
```

To understand that, first let's see what a chord representation looks like:

C Major chord representation

```clojure
{:chord/strings {1 0, 2 1, 3 0, 4 2, 5 3}, :chord/name "C", :chord/fingers {1 2, 2 4, 3 5}}
```

The `:chord/strings` is a map where the key is the string number (starting with 1) and the
value is the fret position, the `:chord/name` is the name and `:chord/fingers` is a map
that where the key is the finger and the value is the string in which that finger must
be played.

On the freboard what the user sets up is just the `:chord/strings`, the other pieces
are deduced from it (taking the tunning in consideration).

Let's explore each of those parts individually

### Deducing the chord name

On music theory, there are some criteria that you can use to find out a chord name once
you know the notes that are being played, the first thing to look at is the `tonic` (that's
the main note of the chord, usually is the lower one, or you can say the one that is on
the higher string number), after that you need to check the distance between the tonic
and the other notes, and with that relationship you can figure out the chord name.

If was going to keep typing I would get too techinical, so instead I'll share with you
the full source that does that, and you can examine if you like:

```clojure
(defn- chord-traits [notes]
  (let [c?   (partial contains? notes)
        sub? #(set/subset? % notes)]
    (->> [(if (sub? #{3 6 9})
            "°"
            [(cond
               (c? 4) nil
               (c? 3) "m"
               (c? 5) (if-not (sub? #{5 7 10}) "4")
               :else "5")
             (if (and (c? 9) (not (c? 10))) "6")])

          (if (sub? #{5 7 10})
            "7sus4"
            (if (c? 10) "7"))
          (if (c? 11) "7M")
          (if (sub? #{4 3}) "9+")
          (if (c? 1) "9-")
          (if (c? 2) "9")
          (if (and (c? 5) (not (sub? #{5 7 10}))) "11")
          (if (and (c? 6) (not (sub? #{3 9}))) "11+")
          (if (c? 8) (if (and (c? 10) (not (c? 3))) "5+" "13-"))
          (if (sub? #{9 10}) "13")]
         (flatten)
         (filter identity))))

(defn strings->notes [tunning strings]
  (->> (sort-by first strings)
       (reverse)
       (map (fn [[string fret]] (note-at tunning string fret)))
       (map normalize)))

(defn notes-for-chord [[tone :as notes]]
  (set (->> (map #(- % tone) notes)
            (map normalize))))

(def ^:private special-cases
  {(:strings standard-tunning) {[9 4 9 11 4] "A9"}})

(defn detect-inverted-bases [tone traits]
  (let [traits (set traits)]
    (case traits
      #{"4" "11" "13-"}
      [(+ tone 5) ["m"] tone]

      #{"4" "6" "11"}
      [(+ tone 5) [] tone]

      #{"5" "7M" "9" "11+"}
      [(dec tone) ["m"] tone]

      #{"m" "7M" "11+"}
      [(dec tone) [] tone]

      [tone traits])))

(defn name-chord
  "Generate a chord name based on a given list of notes."
  ([notes] (name-chord notes (:strings standard-tunning)))
  ([[tone :as notes] tunning]
   (if (seq notes)
     (or
       (get-in special-cases [tunning notes])
       (let [notes  (notes-for-chord notes)
             traits (chord-traits notes)
             [tone traits bass] (detect-inverted-bases tone traits)]
         (str (name (number->tone-sus tone))
              (let [[syms [n1 & rest :as nums]] (split-with #(contains? #{"m" "°"} %) traits)]
                (str (clojure.string/join syms)
                     (case (count nums)
                       0 ""
                       1 n1
                       (str n1 "(" (clojure.string/join "/" rest) ")"))
                     (if bass (str "/" (name (number->tone-sus bass))))))))))))
```

Please keep in mind that this algorithm is on it's first versions, there a lot of
edge cases that are not covered (the recognition of inverted basses for example still
needs lots of improvements).

### Deducing finger position

The fingers position starts simple, when you evaluate a few examples you notice a first
clear pattern about finger positioning, it always goes into the following direction:

![Fingers direction]({{ site.url }}/assets/the-making-of-musicoacher/fret-fingers-direction.png)

And to accomodate that we can do as:

```clojure
(->> (sort-by (juxt second #(- (first %))) strings)
              (filter #(> (second %) 0))
              (map-indexed #(vector (inc %) (first %2)))
              (into {}))
```

But our problems just began, next we have to deal with bare chords, like those:

![Bare Chords]({{ site.url }}/assets/the-making-of-musicoacher/bare-chords.png)

For that the math is a bit more complicated, I'll leave the sources for those that are
into this kind of code, but before let's talk about the last problem, that's finger
distances.

Look at the image with the direction arrows and notice the last example, you gonna
see that it uses the fingers `1`, `2` and `4`, and in order to figure out that the
last finger is a `4` and not a `3` there is also some computation involved (check the
`string-fingers-expand` function), it's all on the following source:

```clojure
(defn strings-max-fret-distance [strings]
  (let [frets (->> (vals strings)
                   (filter #(> % 0)))]
    (- (apply max frets) (apply min frets))))

(defn- string-fingers-bare [strings fingers]
  (let [frets              (->> (u/map-values strings fingers)
                                (group-by second))
        min                (->> (map second strings)
                                (filter #(> % 0))
                                (apply min))
        min-fret           (get frets min)
        first-fret-strings (map (fn [[f]] (fingers f)) min-fret)
        higher-fret        (apply max first-fret-strings)
        same-fret-count    (count min-fret)]
    (if (and (or (> (count fingers) 4)
                 (> (strings-max-fret-distance strings) 2)
                 (> same-fret-count 2))
             (> same-fret-count 1)
             (every? #(> (or (strings %) 1) 0) (range 1 (inc higher-fret))))
      (let [fingers (->> (u/map-keys #(if (> % 1) (- % (dec same-fret-count)) %) fingers)
                         (filter #(> (first %) 0))
                         (into {}))]
        (assoc fingers 1 [1 higher-fret]))
      fingers)))

(defn- distance* [[y1 x1] [y2 x2]]
  (let [dx (- x2 x1) dy (- y2 y1)]
    (.sqrt js/Math (+ (* dy dy) (* dx dx)))))

(defn- distance [a [_ x :as b]]
  (if (vector? a)
    (distance* a b)
    (* 2 (- x a))))

(defn- strings-relationship [strings]
  "Returns a version of the strings as if it was the beginning of the fretboard, you
  end up with a strings version that's the pure shape at the beginning of the fretboard."
  (let [min-string (apply min (keys strings))
        min-note (apply min (vals strings))]
    (->> (sort strings)
         (map (fn [[s n]] [(- s min-string) (- n min-note)]))
         (into {}))))

(defn- power-chord-shape? [strings] (= (strings-relationship strings) {0 2, 1 2, 2 0}))

(defn- string-fingers-expand [strings fingers]
  (if (and (< (count fingers) 4)
           (> (count fingers) 1))
    (if (power-chord-shape? strings)
      (u/map-keys #(if (> % 1) (inc %) %) fingers)
      (let [[finger distance] (->> (u/map-values #(if (vector? %)
                                                   (strings (second %))
                                                   [% (strings %)]) fingers)
                                   (vals)
                                   (partition 2 1)
                                   (map #(apply distance %))
                                   (map vector (drop 2 (range)))
                                   (sort-by second)
                                   last)
            offset (min (- 4 (count fingers)) (.floor js/Math (/ distance 2.5)))]
        (u/map-keys #(if (>= % finger) (+ offset %) %) fingers)))
    fingers))

(defn string-fingers [strings]
  (let [fingers (->> (sort-by (juxt second #(- (first %))) strings)
                     (filter #(> (second %) 0))
                     (map-indexed #(vector (inc %) (first %2)))
                     (into {}))]
    (->> (string-fingers-bare strings fingers)
         (string-fingers-expand strings))))
```

I'm willing to open-source the code that does all this kind of math, I just want it
to be more stable before it, but then would be great to have other minds helping out
here, because it can get tricky.

## Communication

Musicoacher uses 2 different types of communication between the components, the first
are events fired by state/props (props for native components, state for my custom), this
is used for the components that are usually closer to the leaf nodes on the component
tree, these are the pieces that do the real work and know nothing about the app in general,
they just receive props and fire events back, one component that I can use for example
here is the `chord-builder`, which is the componet you use to create chords, here is
an extracted code from it's usage:

```clojure
(om/build chord-builder
          {:value   (get chord-book selected-chord)
           :tunning tunning
           :width   width
           :frets   frets
           :height  (* width 0.21833333)
           :flip-h? flip-board?
           :flip-v? flip-board?}
          {:init-state {:on-change #(u/send owner [:update-chord %])}})
```

This type of communication is good for keeping the components decoupled from the others.

Tip: send the callback functions as `state` instead of `props`, most of the times the callback
functions are created during the render time (like the one on the example before) and
if you send that as a prop it can prevent the `shouldComponentUpdate` to work properly
because the props will be considered different, this is not what you want most of the time,
but you also have to be a bit careful, remember that your function will be created only
once, so if you need to access props from there, remember to use `om/get-props`, otherwise
you will always get the props as the first version when the function was created.

To help on calling methods on state I use this helper function:

```clojure
(defn call-state-fn [owner f & args]
  (if-let [f (om.core/get-state owner f)]
    (apply f args)))
```

The second level uses `core.async`, there is one event bus for the app (that's a `core.async`
channel), and from this bus there is a pub/sub, then I use `services` that are registered
into the pub/sub at component level.

Example setup code to init app bus and pub/sub

```clojure
(defonce bus (async/chan 1024))
(defonce pub (async/pub bus first))
(defonce app-state (atom initial-state))

(om/root app-view
             app-state
             {:shared {:bus bus :pub pub}
              :target (. js/document (getElementById "app"))})
```

What I mean by that is that each of my React components can add more services to listen
for the events placed on the bus, here is an example code of what I mean by components
registering services:

```clojure
(defn run-services [owner]
  (let [pub    (om/get-shared owner :pub)
        cursor (om/get-props owner)]
    [(go-sub pub :some-event [_ event-data]
       (do-something))]))

(defn component-with-services [props owner]
  (reify
    om/IInitState
    (init-state [_] {:services (run-services owner)})

    om/IWillUnmount
    (will-unmount [_] (u/release-subscriptions (om/get-state owner :services)))

    om/IRender
    (render [_]
      (dom/div nil
        "Hello World"))))
```

The `go-sub` macro is a helper that will listen to an event and return some information
about it (this information is used to remove the subscription later).

```clojure
(defmacro go-sub* [pub key binding c & body]
  `(let [ch# ~c]
     (cljs.core.async/sub ~pub ~key ch#)
     (go-loop []
              (when-let [~binding (cljs.core.async/<! ch#)]
                ~@body
                (recur)))
     [~pub ~key ch#]))

(defmacro go-sub [pub key binding & body]
  `(go-sub* ~pub ~key ~binding (cljs.core.async/chan 1) ~@body) )
```

So the `run-services` will return a list of those representations and it is stored on
the component state when it's built, and on the `will-unmount` we use that to remove
the services when the component is detached for any reason.

```clojure
(defn release-subscriptions [subs]
  (doseq [[p t ch] subs] (async/unsub p t ch)))
```

I hope by now it's clear what I mean by services, a component that is inserted on the
tree is capable of listening to any events put on the bus, and any component can place
events on the bus at any time from anywhere, this allows for far components on
the tree to still communicate with little effort, most of the app level communication
goes here, in the root component there are services dedicated for general purpose things
like user sign/out, search, navigation... And each "page component" (components that are
the second higher level, just after the layout one) has it's own services, for example
the Editor component has services to manage everything you do when writing tracks and
they are the ones who communicate with Parse to persist the data, so in general this is
a very flexible model and I end up liking it very much.

This is the helper used to send messages and an example usage of it:

```clojure
(defn send [owner msg]
  (if-let [bus (om/get-shared owner :bus)]
    (put! bus msg)))
    
(send owner [:visit (r/composition-path source-id (:db/id track))])
```

## 60 FPS

In order to move fast some optimizations needs to be in place, when you have something
on movement your goal should always be the 60 FPS, but it's not an easy task, that means
you have about 16ms to do all your processing + render (spoiler: the secret is caching).

Using React and Om the strategy for making your app fasting is making as most components
hit cache as possible, that means, avoid sending data that component doesn't need.

In the case of Musicoacher editor the `time` property is the one that I have to be careful
with, because this property is update on each animation frame, so, even so the time property
comes from the very up, the components that doesn't need it (like the chord book and etc)
don't update on it's change, it's a simple thing that you should always have in mind.

Another tip that I have for you is: don't be afraid of creating more components.
In fact, creating more components is what can boost your speed, when you use just plain
components it has to render to the virtual DOM every time, when you have an Om component
it's different, in this case it will first just compare the data, and if the data matches
(worth remember, ClojureScript uses immutable data structures that can compare immutable
data pretty fast) none of the virtual DOM of that component will be rendered and this way
you can save a lot of your precious CPU cycles, so keep that in mind when organizing
your component tree.

One interesting optimization here is for the Chord Follower component (the component just
on the right side of the video), for that component I a cache info like this:

```clojure
{:current 123 ; the time where the current item point starts
 :index 2 ; the index of the current point
 :next 1234} ; the time for the next time point
```

Using that this function runs on each iteration:

```clojure
(defn step-timeline [timeline time {:keys [current index next] :as cache}]
  (cond
    ; most common case, no changes
    (and index (>= time current) (or (< time next) (nil? next)))
    cache

    ; when we step into the next
    (and cache next (>= time next)
         (let [nnext (get-in timeline [(+ 2 index) :timeline/time])]
           (or (nil? nnext)
               (< time nnext))))
    (let [current (get timeline (inc index))]
      {:current (:timeline/time current)
       :index   (inc index)
       :next    (get-in timeline [(+ 2 index) :timeline/time])})

    ; default case, slow lookup
    :else
    (let [index (timeline-index-at timeline time)
          value (get timeline index)]
      {:current (:timeline/time value)
       :index   index :next (get-in timeline [(inc index) :timeline/time])})))
```

This way, most of the times I just hit cache using a fast check, but it can safely
fall back to other methods.

You also need to be careful about what you are rendering, which CSS properties are you
changing, to know more about that I highly recommend [this free course from Google about
browser rendering optimization](https://www.udacity.com/course/browser-rendering-optimization--ud860),
it will teach you a lot on these things, and more important it will teach you how to use
the profiler tool on Chrome, and in the end, you have to profile and keep retrying until
you get the performance that you need, doing that you can get pretty good results.

## Moving forward

The current annotation system is simple but it's usage can be tiresome, specially for
very repetitive songs, the reason for this method on this release is because it is a simple
one to implement and it's pretty flexible in general.

The plan is to continue working on the current features and make then very easy to use before
move to other features, there are still basic things that are not there like: changing
the tunning for the track, putting a capo on the guitar arm, change play speed, change
play volume...

If you have any feedback on system please report using the Feedback button on the website,
all ideas will be taking in consideration.

## Conclusion

This project is being really fun, the possibilities are almost endless and that ecourages
me to keep working on it.

Thanks for keep on the reading, this ended up a bit bigger than I expected, if you have
any questions just let me know.
