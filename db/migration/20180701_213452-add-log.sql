CREATE TABLE publish_log (
    id serial PRIMARY KEY,
    kind text NOT NULL,
    channel text NOT NULL,
    published_at timestamp with timezone NOT NULL
);

CREATE TABLE called_person (
    log_id integer NOT NULL REFERENCES publish_log,
    person_id integer NOT NULL REFERENCES person
);