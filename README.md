# Reflex JSX

`reflex-jsx` is a relatively simple quasiquoter that lets you write
html-like syntax inside of a reflex `MonadWidget t m` instance.

## Usage

1. Get a build working with Reflex. Due to GHCJS, this isn't as
    trivial as normal libraries. I recommend going through Ryan
    Trinkle's
    [reflex instructions](https://github.com/reflex-frp/reflex-platform#reflex-platform)
    first, and getting a basic app up and running (using the
    `try-reflex` command).
2. From there, you can depend on the `reflex-jsx` library inside of
  `build-depends` of your cabal file as usual, and use the `work-on`
  command from the above platform. It looks something like this:

    ```bash
    /path/to/reflex-platform/work-on ghcjs ./your-project
    cd your-project/src
    ghcjs Main.hs
    ```

  This would work if your code lives in `src` and your main file is
  `Main.hs`. Make sure you invoke `work-on` from the parent directory.

3. At the top of the file, enable quasi-quoting, and import the lib:

   ```haskell
   {-# LANGUAGE QuasiQuotes #-}

   import ReflexJsx
   ```
4. Use the code in a function, like so:

   ```haskell
   mainLayout :: MonadWidget t m => String -> m () ->  m ()
   mainLayout title content = do
     [jsx| <div>
             <div class="header">{text title}</div>
             <div class="body">{content}</div>
           </div>
         |]
    ```

## Examples

### Basic HTML with attributes

Something like:

```html
[jsx| <div class="red" style="color:blue"></div> |]
```

is converted into

```haskell
el "div" (Map.fromList [("class", "red"), ("style", "color:blue")]) $ text ""
```

So there's no restriction on the element type, and the attributes are
`(String, String)` as it is in Reflex itself.

### Splicing in an expression as a node



### Splicing in a `String` attribute



### Splicing in a dynamic set of attributes



## FAQs

### Is this the same JSX as React's version?

The name comes from React's JSX, but it's not trying to be feature-for-feature
mapping of functionality. It's simply meant to play a similar role to JSX in
that it allows the programmer to write HTML-like syntax where it feels
appropriate (i.e. HTML-heavy components that don't have as much interactivity).

I almost called it `hsx` until I discovered that it already had a meaning in Haskell.

I imagine Reflex users will want to use this syntax less often than React users
use JSX, as Reflex's interspersed return values make some types of components
easier to represent with regular functions. I don't recommend this be used all
over the place, because the template haskell slows down compile times, and it
adds a layer of indirection.

### I got a ridiculous looking error

Sorry about that - I actually am not sure how to make the errors nicer. I
essentially just splice in whatever you provide as Template Haskell expressions
using `haskell-src-meta` and let the compiler figure it out if it's not
well-formed. However, sometimes the compiler gets really confused and fails in a
weird way. If anyone knows how to do reliable reflection on TH Exp datatypes to
determine what they are (i.e. `String` vs. `MonadWidget t m`), then I could fail
earlier with a nicer message.

### How stable is this?

Consider it extremely experimental. It's not feature complete, and has only been
used by me a little bit on a side-project, so far from battle tested. Also, this
is my first time working with Quasiquotes and Template Haskell so the
implementation might broken. Do let me know if you run into issues though, and I
will fix them as quickly as possible!

### I have a suggestion for improvement, or want to make a PR

I'm happy to get suggestions on any aspect of the library! I may be doing things unidiomatically
and appreciate getting style, implementation, and any other feedback you want to provide. Even
better if you want to submit a PR to improve the library, but I would hope we can keep it
relatively simple.
