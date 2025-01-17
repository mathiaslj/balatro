% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balatro.R
\name{balatro}
\alias{balatro}
\title{Calculate a balatro score}
\usage{
balatro(
  cards,
  base_score = balatro_score(),
  jokers = NULL,
  card_buffs = NULL,
  debuff = NULL,
  ...,
  deck_format = build_deck()
)
}
\arguments{
\item{cards}{a \code{character} vector of card names}

\item{base_score}{a \code{balatro_score}}

\item{jokers}{a \code{vector} or \code{list} of calls to \code{chips}, \code{multp}
or \code{multx}
and multx functions}

\item{card_buffs}{like \code{joker} a collection of calls to}

\item{debuff}{a \code{character} specifying what type of cards are debuffed.
Fx. \code{debuff = "odd"}, \code{debuff = "hearts"}, etc.}

\item{...}{Nothing for now}

\item{deck_format}{the result of a call to \code{build_deck}. At the moment,
the default call to \code{build_deck} builds a deck with an assumed format
that is needed for other functionalities. Do not touch this argument.}
}
\value{
a \code{numeric} with the score
}
\description{
Scores are generated in the order of operations:
\itemize{
\item Base score based on level of hand
\itemize{
\item Specify with \code{base_score} argument
}
\item Chip count from cards are added
\itemize{
\item Taken from the specification of
\code{cards}
}
\item Chip and mult buffs from cards are added in the order
\enumerate{
\item played cards,
\item cards held in hand
}
\itemize{
\item Specify these using \code{card_buffs} (make sure to order them correctly
according to left to right of played cards and held cards)
}
\item Chip and mult from jokers
\itemize{
\item Specify using \code{jokers} argument
\item Foil (\code{chips}), holographic (\code{multp}) and polychrome (\code{multx})
should also just be passed on in this argument
}
}
}
\details{
Use fx. \code{debuff = "hearts"} to debuff hearts so they produce no chip
value and do not have any characteristic towards a joker's scoring.
Fx. if you have a joker that gives mult to even cards but you have a
10 of hearts with hearts debuffed, that 10 won't give mult from the joker
}
\examples{
# Checking the value of a level 1 "flush" with no jokers
balatro(base_score = balatro_score(chips = 35, mult = 4),
        cards = c("qc", "9c", "8c", "4c", "2c"))

# Level 3 "three of a kind" with odd_todd joker that gives 31 chips
# for odd ranked cards and 4+ mult on 2 of the playing cards while
# clubs are debuffed
balatro(base_score = balatro_score(chips = 70, mult = 7),
        cards = c("7d", "7d", "7c"),
        jokers = list(chips(31, card_type = "odd")),
        card_buffs = list(multp(2*4)),
        debuff = "clubs")

# Level 14 "five of a kind" with 10s with jokers adding their bonuses
# up to 118 +mult and 1.5*2.8*2.2*3*3 Xmult. All played cards are glass
# cards, giving 2X mult, and 3 steel cards are held in hand, each giving
# 1.5X mult
balatro(base_score = balatro_score(chips = 575, mult = 51),
        cards = c(rep("10d", 4), "10h"),
        jokers = list(multp(118), multx(1.5*2.8*2.2*3*3)),
        card_buffs = list(multx(2^5), multx(1.5^3)))

}
