# predictions of bitcoin prices by 50 experts

library(ggplot2)
library(reshape2)

# the price of bitcoin, Feb. 25th 2015
p_bitcoin = 238.31


# yes, I did this manually. :(
predictions = rbind(
    c(35000, 35000),
    c(1000, 5000),
    c(5000, 5000),
    c(5000, 5000),
    c(2500, 2500),
    c(5000, 5000),
    c(3000, 3000),
    c(NA, NA),
    c(2000, 2000),
    c(10000, 100000),
    c(50000, 50000),
    c(10000, 10000),
    c(10000, 10000),
    c(10000, 10000),
    c(898.13, 898.13),
    c(10000, 10000),
    c(1000, 3000), # 'low thousands'
    c(5000, 5000),
    c(5000, 10000),
    c(3000, 4000),
    c(1066, 1066),
    c(2200, 3400),
    c(1200, 1200),
    c(800, 2000),
    c(25000, 25000),
    c(8000, 10000),
    c(4500, 4500),
    c(50, 10000),
    c(10000, 10000),
    c(1800, 1900),
    c(10000, 10000), # '5-digit-amounts'
    c(500, 10000),
    c(6500, 6500),
    c(1500, 1500),
    c(1100, 1300),
    c(4000, 5000),
    c(2000, 5000),
    c(7499, 7499),
    c(NA, NA), # 'an all-time high of at least triple this year’s
               # all-time high'
    c(NA, NA), # 'Depends on Wallstreet and Regulators.'
    c(10000, 10000),
    c(4000, 4000),
    c(1000, NA), # 'I am very confident it will be much higher than it
                 # is now. [$534.71]'
    c(5000, 5000),
    c(NA, NA), # 'the honest truth is that nobody knows what it will be like'
    c(5000, 5000),
    c(5000, 5000),
    c(2860.23, 2860.23)
)


names = c(
    "Ken Lo, CEO of ANX",
    "Adrian, Founder of SatoshiBet",
    "Kenneth Metral, CEO of Coingig",
    "Kingsley Edwards, Founder of LeetCoin",
    "Jonathan Speigner, CEO of LiteTree",
    "Mike Yeung, Founder of SFU Bitcoin Club",
    "Jaron Lukasiewicz, CEO of Coinsetter",
    "Alexander Lawn, director of KnCMiner",
    "Alan Silbert, CEO of BitPremier",
    "Mrs P, the founder of The Bitcoin Wife",
    "Mark Norton, from Bitcoin Warrior",
    "Francisco Tomas Buero, Co-Founder of Conectabitcoin",
    "Nubis Bruno, Co-Founder of Conectabitcoin",
    "Rodolfo Novak, Founder of Coinkite",
    "Alan Donohoe, Founder of Bitcoin Association of Ireland",
    "Zach Harvey, CEO of Lamassu",
    "Vitalik Buterin, Author at Bitcoin Magazine",
    "Frank Schuil, CEO of Safello",
    "Tony Tam, Co-Founder of Bitcoin Pulse",
    "Flavio Rump, Co-Founder of Bitcoin Pulse",
    "Daniel, Founder of Dagensia",
    "Simon Edhouse, Managing Director of Bittunes",
    "Peter Seed, CEO of Tradefor",
    "Developer/Webmaster of CryptoHits",
    "Morgan Rockwell, Founder of Bitcoin Kinetics",
    "Ash Moran, Co-Founder of Bitcoin Manchester",
    "Max, Co-Founder of Bitcoin Manchester",
    "Alena Vranova, CEO of SatoshiLabs (TREZOR)",
    "Ron Gross, Mastercoin Foundation Executive Director",
    "Vytautas Karalevičius, CEO of Spectro Coin",
    "Dominik Weil, Co-Foudner of Bitcoin Azerbaijan",
    "J.R. Willett, Founder of Mastercoin",
    "Kevin Barnes, CEO of Playcoin Entertainment",
    "Teemu Päivinen, CEO of Coinmotion",
    "Keny, Founder of GetToKnowBitcoin",
    "Michal Handerhan, CEO of BitcoinShopUS",
    "Wouter Vonk, Founder of Bitgild",
    "Gabriel Miron, Founder of MEXBT.com",
    "Esther Tung at the The Bitcoin Co-op",
    "Antony, Business Development at itBit",
    "John Delono, Founder of Bitcoin Reviewer",
    "Eddy Travia, Co-Founder of Seedcoin",
    "Johnathan Turrall, CTO of MetaLair",
    "Nikos Bentenitis, Founder of CoinSimple",
    "Frederic Thenault, Founder of iceVault",
    "Aaron Williams, Founder of Atlanta Bitcoin",
    "Michael Dunworth, CEO of snapCard",
    "Daniel Mross, Director of The Rise and Rise of Bitcoin"
)

colnames(predictions) = c("Low", "High")
rownames(predictions) = names



# How many of the low end predictions are at least an order of
# magnitude too high? 29!
sum(predictions[, 1] > p_bitcoin * 10, na.rm = T)




pm = melt(predictions)
pm$Var2 = factor(pm$Var2, levels=c("Low", "High"),
    labels=c("Low End", "High End"))
names(pm) = c("Expert", "Range", "Prediction")


# convert to multiples of actual bitcoin price
pm$Prediction = pm$Prediction / p_bitcoin


# plot
png("expert_predictions.png", height = 600, width = 1000, res = 100)
ggplot(pm, aes(x = Prediction)) +
  stat_density(adjust=.1, aes(fill = Range,
                   ymax = ..density..,  ymin = -..density..),
               geom = "ribbon", position = "identity") +
                 labs(title = "\"Expert\" Predictions\n",
                      x = "Multiples of actual Bitcoin price ($238.31)\nBlack line = 1",
                      y = "Density") +
                        scale_fill_manual(
                            values=c(rgb(.2,.2,.8,.6), rgb(1,0,0,.6))) +
                              #xlim(c(0, 45)) +
                                geom_vline(xintercept = 1)
dev.off()
