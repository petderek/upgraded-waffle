## How?

```
docker build -t rkt .
docker run -it rkt

# once inside the container -- run like this

cd other
racket ${pick a file}

# I'm incredibly lazy, so all of these challenge puzzles assume that the input
# text is in the same directory. cd around to run other things

cd ../advent
racket ${pick a different file}
```

## Why is it called upgraded-waffle?

Github has a name generator and I trusted it was better at its job than me.
