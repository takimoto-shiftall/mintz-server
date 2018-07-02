CREATE TABLE publish_log (
    id serial PRIMARY KEY,
    kind text NOT NULL,
    channel text NOT NULL,
    message text NOT NULL,
    voice text NOT NULL,
    audio_hash text NOT NULL,
    published_at timestamp with time zone NOT NULL
);

CREATE TABLE called_person (
    log_id integer NOT NULL REFERENCES publish_log,
    person_id integer NOT NULL REFERENCES person
);