package com.daml.platform.store.dao.events

case class PreparedRawEntry(tx: TransactionIndexing.TransactionInfo,
                            events: TransactionIndexing.EventsInfo,
                            compressed: TransactionIndexing.Compressed,
                            contracts: TransactionIndexing.ContractsInfo,
                            contractWitnesses: TransactionIndexing.ContractWitnessesInfo
                           )
