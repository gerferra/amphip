/*
 * "Farmer's problem" based on 
 * http://dx.doi.org/10.1007/978-1-4614-0237-4.
 */


import amphip.dsl._

import spire.implicits._

import amphip.stoch.BasicScenario

object farmer {

  object EV {
    val c      = dummy
    val Crops  = set
    val p      = dummy
    val PCrops = set within Crops // purchasable crops
    val f      = dummy
    val FCrops = set within Crops &~ PCrops // saleable crops at favorable price
                                            // FCrops ∩ PCrops = ∅

    val totalLand     = param
    val requirement   = param(PCrops)
    val yields        = param(Crops)
    val plantingCost  = param(Crops)
    val purchasePrice = param(PCrops)
    val sellingPrice  = param(Crops)
    val sellingFPrice = param(FCrops)
    val fQuota        = param(FCrops)

    val x  = xvar(Crops)  >= 0 // land devoted to each crop
    val w  = xvar(Crops)  >= 0 // crops sold at normal price
    val wf = xvar(FCrops) >= 0 // crops sold at favorable price 
    val y  = xvar(PCrops) >= 0 // purchased crops
    
    val availableLand = st { 
      sum(c in Crops) { x(c) } <= totalLand
    }

    val productionP = st(p in PCrops) {
      yields(p) * x(p) + y(p) - w(p) >= requirement(p)
    }

    val productionF = st(f in FCrops) { 
      w(f) + wf(f) <= yields(f) * x(f) 
    }

    // For completeness. With the given instance this is the empty set
    val productionC = st(c in Crops &~ (PCrops | FCrops)) {
      w(c) <= yields(c) * x(c)
    }

    val fpriceQuota = st(f in FCrops) { wf(f) <= fQuota(f) }

    val benefit = maximize { 
      sum(c in Crops ) { sellingPrice (c) * w (c) } +
      sum(f in FCrops) { sellingFPrice(f) * wf(f) } -
      sum(c in Crops ) { plantingCost (c) * x (c) } - 
      sum(p in PCrops) { purchasePrice(p) * y (p) } 
    }
    
    val evModel = model(benefit, 
      availableLand, 
      productionP, productionF, productionC, 
      fpriceQuota)
    
    // data 

    val W = "Wheat"
    val C = "Corn"
    val B = "Beet"
    
    val evModelWithData = evModel 
      .setData(Crops, List(W, C, B))
      .setData(PCrops, List(W, C))
      .setData(FCrops, List(B))
      .paramData(totalLand, 500)
      .paramData(requirement,
        W -> 200, 
        C -> 240)
      .paramData(yields, 
        W ->  2.5,
        C ->  3d,
        B -> 20d)
      .paramData(plantingCost,
        W -> 150,
        C -> 230,
        B -> 260)
      .paramData(purchasePrice, 
        W -> 170 * 1.4, // 238
        C -> 150 * 1.4) // 210
      .paramData(sellingPrice, 
        W -> 170,
        C -> 150,
        B ->  10)
      .paramData(sellingFPrice, B -> 36)
      .paramData(fQuota, B -> 6000)

    val (sout, out) = evModelWithData.solve // 118600
  }

  trait RPBase {
    import EV._

    val s = dummy
    val S = set
    
    val yields = param(S, Crops)
    val pi     = param(S)

    val w  = xvar(S, Crops)  >= 0
    val wf = xvar(S, FCrops) >= 0
    val y  = xvar(S, PCrops) >= 0

    val productionP = st(s in S, p in PCrops) {
      yields(s,p) * x(p) + y(s,p) - w(s,p) >= requirement(p)
    }

    val productionF = st(s in S, f in FCrops) { 
      w(s,f) + wf(s,f) <= yields(s,f) * x(f) 
    }

    val productionC = st(s in S, c in Crops &~ (PCrops | FCrops)) {
      w(s,c) <= yields(s, c) * x(c)
    }

    val fpriceQuota = st(s in S, f in FCrops) { wf(s, f) <= fQuota(f) }

    val benefit = maximize { sum(s in S) { pi(s) * (
        sum(c in Crops ) { sellingPrice (c) * w (s,c) } +
        sum(f in FCrops) { sellingFPrice(f) * wf(s,f) } -
        sum(c in Crops ) { plantingCost (c) * x (c)   } - 
        sum(p in PCrops) { purchasePrice(p) * y (s,p) } 
      )}
    }
    
    val rpModel = model(benefit, 
                    availableLand, 
                    productionP, productionF, productionC, 
                    fpriceQuota
                  )
  }

  object RP extends RPBase {
    import EV._

    val evData = evModelWithData.data

    val SA = BasicScenario("Above")
    val SM = BasicScenario("Medium")
    val SB = BasicScenario("Below")

    val stochRPModel = rpModel.stochastic(S, pi)
      .setDataList(List(
        Crops  -> evData.set(Crops),
        PCrops -> evData.set(PCrops),
        FCrops -> evData.set(FCrops)
      ))
      .paramDataList(List(
        totalLand     -> evData.param(totalLand),
        requirement   -> evData.param(requirement),
        plantingCost  -> evData.param(plantingCost),
        purchasePrice -> evData.param(purchasePrice),
        sellingPrice  -> evData.param(sellingPrice),
        sellingFPrice -> evData.param(sellingFPrice),
        fQuota        -> evData.param(fQuota),
      ))
      // new data
      .stochCustomScenarios(Nil, 
        SA -> r"1/3",
        SM -> r"1/3",
        SB -> r"1/3")
      .stochScenarioData(yields, List(SA), 
        W ->  2.5 * 1.2, //  3
        C ->  3   * 1.2, //  3.6
        B -> 20   * 1.2) // 24
      .stochScenarioData(yields, List(SM), 
        W ->  2.5,
        C ->  3d,
        B -> 20d)
      .stochScenarioData(yields, List(SB),
        W ->  2.5 * 0.8, //  2
        C ->  3   * 0.8, //  2.4
        B -> 20   * 0.8) // 16
      
    val (sout, out) = stochRPModel.solve // 108390
  }

  object WS {
    val (sout, out) = RP.stochRPModel.separate.solve // 115406
  }

  object RP_byHand extends RPBase {
    import EV._

    val evData = evModelWithData.data

    val SA = "Above"
    val SM = "Medium"
    val SB = "Below"

    val rpModelWithData = rpModel
      .setDataList(List(
        Crops  -> evData.set(Crops),
        PCrops -> evData.set(PCrops),
        FCrops -> evData.set(FCrops)
      ))
      .paramDataList(List(
        totalLand     -> evData.param(totalLand),
        requirement   -> evData.param(requirement),
        plantingCost  -> evData.param(plantingCost),
        purchasePrice -> evData.param(purchasePrice),
        sellingPrice  -> evData.param(sellingPrice),
        sellingFPrice -> evData.param(sellingFPrice),
        fQuota        -> evData.param(fQuota),
      ))
      // new data
      .setData(S, List(SA, SM, SB))
      .paramData(yields, 
        (SA, W) ->  2.5 * 1.2, //  3
        (SA, C) ->  3   * 1.2, //  3.6
        (SA, B) -> 20   * 1.2, // 24

        (SM, W) ->  2.5,
        (SM, C) ->  3d,
        (SM, B) -> 20d,

        (SB, W) ->  2.5 * 0.8, //  2
        (SB, C) ->  3   * 0.8, //  2.4
        (SB, B) -> 20   * 0.8) // 16 
      .paramData(pi, 
        SA -> 1d/3, 
        SM -> 1d/3, 
        SB -> 1d/3, 
      )
      
    val (sout, out) = rpModelWithData.solve
  }
}