# simulator.py
import numpy as np
import pandas as pd

class RiskSimulator:
    def __init__(self, 
                 n, p,
                 fixed_cost,
                 variable_guest_cost,
                 guest_base, 
                 budget, 
                 risk_tolerance,
                 k = 10000,
                 distribution='uniform',
                 seed=420):
                 
        # costs
        self.fixed_cost = fixed_cost
        self.variable_guest_cost = variable_guest_cost
        self.guest_base = guest_base
        self.budget = budget
        self.risk_tolerance = risk_tolerance
        
        # simulation
        self.k = int(k) # reticulate will throw an error with range function
        self.n = n
        self.p = p
        self.distribution = distribution
        self.seed = seed
        
        # results
        self.simulation_data = None
        self.financial_upside = None
        self.financial_risk = None
        self.overall_expectation = None
        self.recommendation = None

    def run(self):
        
        # set seed for reproducibility
        np.random.seed(self.seed)
        
        # determine which probability distribution to sample from
        if self.distribution == 'uniform':
            p_sample_dist = np.random.uniform(
                self.p['low'], self.p['high']
            )
            
        elif self.distribution == 'triangular':
            p_sample_dist = np.random.triangular(
                self.p['left'], self.p['mode'], self.p['right']
            )
        
        else:
            p_sample_dist = 0.75
        
        # simulate k weddings
        total_guests = [
            self.generate_guest_sample(
                n = self.n, 
                p = p_sample_dist
            )[0]
            for i in range(self.k)
        ]
        
        # save results in pandas dataframe (could just use dictionary)
        df = pd.DataFrame({
            'total_guests': total_guests
        })
        
        # calculate the total wedding cost for each sample
        df['total_cost'] = self.calculate_wedding_cost(
            self.fixed_cost, 
            self.variable_guest_cost,
            self.guest_base,
            df['total_guests']
        )
        
        # calculate risk for each sample                                         
        df['risk'] = self.calculate_risk(self.budget, df['total_cost'])
        
        # determine if over budget for each sample
        df['over_budget'] = np.where(
            df['risk'] < 0, "Yes", np.where(df['risk'] > 0, "No", "Even")
        )
        
        # generate recommendation for each sample
        df['recommendation'] = self.calculate_recommendation(df['risk'])
        
        # save results
        self.simulation_data = df
        self.financial_upside = self.calculate_upside_potential()
        self.financial_risk = self.calculate_downside_risk()
        self.overall_expectation = self.calculate_overall_expectation()
        self.recommendation = self.get_recommendation()
        
        # confirm run 
        # print("Simulation completed. See results.")
    
    @staticmethod
    def generate_guest_sample(n, p):
        return np.random.binomial(n, p, size=1)

    def get_recommendation(self):
        
        # ensure simulation has been run
        if self.simulation_data is not None:
        
            # extract risk dictionary for brevity
            risk = self.financial_risk
            
            # determine if risk percentage exceeds tolerance
            recommendation = np.where(
                risk['downside_probability'] > self.risk_tolerance,
                "Invite Less", "Invite All"
            )
            
            return recommendation
        
        else:
            return "ERROR: Simulation not run."
           
    def calculate_upside_potential(self):
        # probability of positive result
        under_budget_probability = np.mean(self.simulation_data['risk'] >= 0)
        # conditional expectation of positive result
        under_budget_payoff = np.mean(
            self.simulation_data.loc[self.simulation_data['risk'] >= 0, 'risk']
        )
        
        # expectation of positive result
        under_budget_expectation = under_budget_probability * \
            under_budget_payoff
            
        # save results in dictionary
        upside_dict = {
            'upside_probability': under_budget_probability,
            'upside_payoff': under_budget_payoff,
            'upside_expectation': under_budget_expectation
        }
    
        return upside_dict

    def calculate_downside_risk(self):
        # probability of negative result
        over_budget_probability = np.mean(self.simulation_data['risk'] < 0)
        
        # conditional expectation of negative result
        over_budget_payoff = np.mean(
            self.simulation_data.loc[self.simulation_data['risk'] < 0, 'risk']
        )
        
        # expectation of positive result
        over_budget_expectation = over_budget_probability * over_budget_payoff
    
        # save results in dictionary
        downside_dict = {
            'downside_probability': over_budget_probability,
            'downside_payoff': over_budget_payoff,
            'downside_expectation': over_budget_expectation
        }
    
        return downside_dict

    def calculate_overall_expectation(self):
        
        # extract dictionary objects
        upside_dict = self.financial_upside
        downside_dict = self.financial_risk
        
        # add the expectations together for overall expected value
        overall_expectation = downside_dict['downside_expectation'] + \
            upside_dict['upside_expectation']

        return overall_expectation

    @staticmethod
    def calculate_wedding_cost(fixed_cost,
                               variable_guest_cost,
                               guest_base,
                               total_guests):
        # cost per additional guest
        variable_cost = variable_guest_cost * (total_guests - guest_base)
        # add to fixed cost
        total_cost = variable_cost + fixed_cost
        return total_cost

    @staticmethod
    def calculate_risk(budget, total_cost):
        return budget - total_cost

    @staticmethod
    def calculate_recommendation(risk):
        return np.where(risk >= 0, "Invite All", "Invite Less")
