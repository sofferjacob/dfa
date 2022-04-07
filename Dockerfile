FROM racket/racket
WORKDIR /app
COPY main.rkt .
CMD ["racket", "-it", "main.rkt"]