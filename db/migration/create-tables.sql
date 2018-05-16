\c mintz

DROP TABLE IF EXISTS employee_department;
DROP TABLE IF EXISTS department;
DROP TABLE IF EXISTS employment;
DROP TABLE IF EXISTS company;
DROP TABLE IF EXISTS person;

DROP TYPE IF EXISTS label;
DROP TYPE IF EXISTS lang;

CREATE TYPE label AS (
    en text,
    mb text,
    en_reading text, 
    mb_reading text
);

CREATE TYPE lang AS (
    en text,
    mb text
);

CREATE TABLE person (
    id serial PRIMARY KEY,
    first_name label NOT NULL,
    middle_name label NOT NULL,
    last_name label NOT NULL,
    description lang NOT NULL,
    notifications jsonb not NULL
);

CREATE TABLE company (
    id serial PRIMARY KEY,
    name label NOT NULL,
    description lang NOT NULL
);

CREATE TABLE employment (
    company_id integer NOT NULL REFERENCES company,
    person_id integer NOT NULL REFERENCES person,
    position lang NULL,
    PRIMARY KEY (company_id, person_id)
);

CREATE TABLE department (
    id serial NOT NULL,
    company_id integer NOT NULL REFERENCES company,
    parent_id integer NULL,
    name label NOT NULL,
    description lang NOT NULL,
    PRIMARY KEY (id, company_id),
    FOREIGN KEY (parent_id, company_id) REFERENCES department (id, company_id)
);

CREATE TABLE employee_department (
    company_id integer NOT NULL,
    person_id integer NOT NULL,
    department_id integer NOT NULL,
    FOREIGN KEY (company_id, person_id) REFERENCES employment (company_id, person_id),
    FOREIGN KEY (company_id, department_id) REFERENCES department (company_id, id)
);