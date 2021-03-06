% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Generics.R
\docType{methods}
\name{trackr_lookup}
\alias{trackr_lookup}
\alias{trackr_lookup,ANY,ANY-method}
\alias{trackr_lookup,character,ANY-method}
\alias{trackr_lookup,character,DocCollectionRef-method}
\alias{trackr_lookup,character,TrackrDB-method}
\alias{insert_record}
\alias{insert_record,ANY-method}
\alias{insert_record,DocCollection-method}
\alias{insert_record,DocCollectionRef-method}
\alias{insert_record,TrackrDB-method}
\alias{prep_for_backend}
\alias{prep_for_backend,FeatureSet,ANY-method}
\alias{prep_for_backend,ObjFeatureSet,ANY-method}
\alias{prep_for_backend,FeatureSet,TrackrDB-method}
\alias{remove_record}
\alias{remove_record,ANY,TrackrDB-method}
\alias{remove_record,character,ANY-method}
\alias{remove_record,character,DocCollectionRef-method}
\alias{remove_record,character,TrackrDB-method}
\alias{trackr_write}
\alias{trackr_write,ANY-method}
\alias{trackr_write,JSONBackend-method}
\alias{trackr_write,TrackrDB-method}
\alias{trackr_search}
\alias{trackr_search,character,DocCollection-method}
\alias{trackr_search,character,DocCollectionRef-method}
\alias{trackr_search,character,SolrList-method}
\alias{trackr_search,character,TrackrDB-method}
\title{Backend-interface API}
\usage{
trackr_lookup(object, target, opts, exist = FALSE)

insert_record(object, id, target, opts, verbose = FALSE)

prep_for_backend(object, target, opts, verbose = FALSE)

remove_record(object, target, opts, verbose = FALSE)

trackr_write(target, opts, verbose = FALSE)

trackr_search(
  pattern,
  target,
  opts,
  fields = NULL,
  ret_type = c("doclist", "id", "backend"),
  verbose = TRUE
)
}
\arguments{
\item{object}{ANY. The object to lookup, add, remove, etc.}

\item{target}{ANY. The backend of the TrackrDB instance.}

\item{opts}{TrackrOptions. The trackr-level options. Typically
extracted from \code{target} in a \code{TrackrDB} method and passed down.}

\item{exist}{logical. Return TRUE/FALSE rather than the looked-up object.
(default: FALSE)}

\item{id}{character. The id to assign to the recorded result within
the target backend.}

\item{verbose}{logical. Should extra informative messages be
displayed ( default: FALSE)}

\item{pattern}{character. A regular expression to match against the
text in \code{fields}}

\item{fields}{character or NULL. The fiends in which to match, or
NULL to include all fields.}

\item{ret_type}{character. Format in which to return the
response. Options are: "id" - id of matching documents
(default), "doclist" - A list containing the matching documents
represnted as R lists, and "backend" - a backend specific
representation of the set of matching documents (generally the
same class as \code{backend}.}
}
\value{
for \code{trackr_lookup}: If \code{exist} is TRUE, a
    logical indicating whether \code{object} was found in
    \code{db}. Otherwise, the object stored in the database (or
    NULL if it was not found).

for \code{insert_record} and \code{remove_record}: The
    \code{TrackrDB} (\code{db} parameter) after the plot has been
    added or removed.

For \code{prep_for_backend}, \code{object}, representend in
    the form that the \code{insert_record} method for
    \code{backend} expects.
}
\description{
This page describes the API which must be implimented by all Trackr backends
Any class with methods for these generic can be used in the \code{backend}
slot of a \code{TrackrDB} object. These functions should not be called directly by an end-user.
}
\details{
Methods should be written to be dispatched on \code{target}, and, where appropriate
on code{object}/code{doc}

These methods should be endomorphic with respect to the
\code{target} argument. That is, they should return an object of
the same class as was passed to \code{target}, which reflect the
requested change to the backend state.

\code{insert_record} may or may not involve writing to
    disk, which can alternatively occur during
    \code{trackr_write}. Writing, if any is desired, must occur
    within at one and only one of these methods. If
    \code{insert_record} performs the writing, \code{trackr_write}
    should be a no-op.
}
\note{
\code{remove_record} should have the same writing behavior
as \code{insert_record}

\code{remove_record} should have the same writing behavior as
    \code{insert_record}
}
