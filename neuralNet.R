setwd("~/CourseraMachineLearning/machine-learning-ex4/ex4")

# %% Setup the parameters you will use for this exercise
input_layer_size  = 400;  #% 20x20 Input Images of Digits
hidden_layer_size = 25;   #% 25 hidden units
num_labels = 10;          #% 10 labels, from 1 to 10   
#% (note that we have mapped "0" to label 10)


X <- read.csv("X.csv",header = FALSE)
y <- read.csv("y.csv",header = FALSE)
Theta1 <- read.csv("Theta1.csv",header = FALSE)
Theta2 <- read.csv("Theta2.csv",header = FALSE)


X <-  as.matrix(X)
y <- as.matrix(y)
Theta1 <- as.matrix(Theta1)
Theta2 <- as.matrix(Theta2)

displayData <- function(X){

  m <- dim(X)[1]
  n <- dim(X)[2]


  example_width <- round(sqrt(n))
  
  example_height <- n/example_width
  
  
  # Compute number of items to display
  display_rows = floor(sqrt(m))
  display_cols = ceiling(m / display_rows)
  
  # Between images padding
  pad = 1;
  
  #options(digits=3)
  
  #image(xMat, axes = FALSE, col = grey(seq(0, 1, length = 256)))
  

  # Setup blank display
  display_array <- matrix(0, nrow= pad + display_rows * (example_height + pad),
                          ncol=pad + display_cols * (example_width + pad))
  
  
  # Copy each example into a patch on the display array
  curr_ex = 1;
  for(j in 1:display_rows) {
    for(i in  1:display_cols) {
        if (curr_ex > m) 
                  break 
  
        #Copy the patch
  
        # Get the max value of the patch
        maxValue <- max(abs(X[curr_ex,]))
        display_array[pad + (j - 1) * (example_height + pad) + (1:example_height),
                pad + (i - 1) * (example_width + pad) + (1:example_width)] <- 
             (matrix(as.vector(X[curr_ex,]), example_height, example_width)/maxValue  + 1)/2
    
        curr_ex = curr_ex + 1;
     }
     if (curr_ex > m) 
              break 
  
  }
  display_array <- t(display_array)[,nrow(display_array):1]

  # Display Image
  image(display_array, axes = FALSE, col = grey(seq(0, 1, length = 256)))
  
}



sel <- sample(nrow(X), 100)
displayData(X[sel,])

# Unroll parameters 

# m1 <- as.matrix(Theta1)
# system.time(replicate(100,
# nn_params <- c(as.vector(as.matrix(Theta1)), as.vector(as.matrix(Theta2)))
# ))
#
# system.time(replicate(100,
#                       nn_par <- c(unlist(Theta1, use.names = F), unlist(Theta2,use.names = F))
# ))


nn_params <- c(as.vector(Theta1), as.vector(Theta2))

#
# ================ Part 3: Compute Cost (Feedforward) ================
#   To the neural network, you should first start by implementing the
#  feedforward part of the neural network that returns the cost only. You
#  should complete the code in nnCostFunction.m to return cost. After
#  implementing the feedforward to compute the cost, you can verify that
#  your implementation is correct by verifying that you get the same cost
#  as us for the fixed debugging parameters.
#
#  We suggest implementing the feedforward cost *without* regularization
#  first so that it will be easier for you to debug. Later, in part 4, you
#  will get to implement the regularized cost.
#

cat("\n Feedforward Using Neural Network ...\n")

sigmoid <- function(z){
  1.0 / (1.0 + exp(-z));
}



nnCostFunction <-function(nn_params,
                                   input_layer_size=400,
                                   hidden_layer_size=25,
                                   num_labels=10,
                                   X, y, lambda){
  
  idx.1 <- 1:(hidden_layer_size * (input_layer_size + 1))
  Theta1 <- matrix(nn_params[idx.1],
                   hidden_layer_size, (input_layer_size + 1))
  
  Theta2 <- matrix(nn_params[-idx.1],
                   num_labels, (hidden_layer_size + 1))
  
  # Setup some useful variables
  m <- dim(X)[1]
  
  # You need to return the following variables correctly 
  J <- 0;


  # Part 1: Feedforward the neural network and return the cost in the
  #         variable J. After implementing Part 1, you can verify that your
  #         cost function computation is correct by verifying the cost
  #         computed in ex4.m
  #
  # Part 2: Implement the backpropagation algorithm to compute the gradients
  #         Theta1_grad and Theta2_grad. You should return the partial derivatives of
  #         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
  #         Theta2_grad, respectively. After implementing Part 2, you can check
  #         that your implementation is correct by running checkNNGradients
  #
  #         Note: The vector y passed into the function is a vector of labels
  #               containing values from 1..K. You need to map this vector into a 
  #               binary vector of 1's and 0's to be used with the neural network
  #               cost function.
  #
  #         Hint: We recommend implementing backpropagation using a for-loop
  #               over the training examples if you are implementing it for the 
  #               first time.
  #
  # Part 3: Implement regularization with the cost function and gradients.
  #
  #         Hint: You can implement this around the code for
  #               backpropagation. That is, you can compute the gradients for
  #               the regularization separately and then add them to Theta1_grad
  #               and Theta2_grad from Part 2.
  #
  
  # Part 1:==================================
    
    X.1 <- cbind( matrix(1,m,1), X )
    colnames(X.1) <- NULL
    Y <- matrix(0, m, num_labels)
    A1 <- X.1
  # a2' = (Theta1 * a1)' = a1' * Theta1' => X * Theta1' 
    A2 <-(sigmoid(X.1 %*% t(Theta1)))
    A2.1 <- cbind( matrix(1,m,1), A2)
    A3 <- sigmoid(A2.1 %*% t(Theta2))
    
    for(k in 1: num_labels){
      Y[,k]  <- (y == k)
      J <- J - 1/m *  ( t(Y[,k]) %*% log(A3[,k]) +
                    + (1 - t(Y[,k])) %*% log(1 - A3[,k])) 
    }
    
    #%  Implement regularization --------------------------------------------
    #% remove bias column. Theta1(:, 2:end) and then...
    #J += lambda/(2*m)*(sum(Theta1(:,2:end)(:).^2) + sum(Theta2(:,2:end)(:).^2));
    J <- J + lambda /(2*m)*(sum(as.vector(Theta1[,-1])^2) + sum(as.vector(Theta2[,-1])^2))
    J
}

nnGradFunction <-function(nn_params,
                          input_layer_size=400,
                          hidden_layer_size=25,
                          num_labels=10,
                          X, y, lambda){
  
  idx.1 <- 1:(hidden_layer_size * (input_layer_size + 1))
  Theta1 <- matrix(nn_params[idx.1],
                   hidden_layer_size, (input_layer_size + 1))
  
  Theta2 <- matrix(nn_params[-idx.1],
                   num_labels, (hidden_layer_size + 1))
  
  # Setup some useful variables
  m <- dim(X)[1]
  
    
#% Part 2: =====================================
#  % Implement the backpropagation algorithm to compute the gradients
#         Theta1_grad and Theta2_grad.  
   Delta2 <- 0
   Delta1 <- 0
   
   
   X.1 <- cbind( matrix(1,m,1), X )
   colnames(X.1) <- NULL
   Y <- matrix(0, m, num_labels)
   
   
   for(k in 1: num_labels){
     Y[,k]  <- (y == k)
   }

   for(i in 1:m){
     a1.1 <- X.1[i,]   # this subset is a vector not a matrix. X[i,]is the same as t(X[i,,drop=F])....  
     a2 <- sigmoid(Theta1 %*% a1.1)
     # add the bias factor into a2.
     a2.1 <- rbind(1, a2) 
     a3 <- sigmoid(Theta2 %*% a2.1) 
   
   
     d3 <- a3 - Y[i, ]  # this subset is a vector not a matrix. Y[i,] is the same as t(Y[i,,drop=F]).
     d2.1 <- t(Theta2) %*% d3 * a2.1 * (1 - a2.1) 
     # % remove bias factor d2(1)
   
     d2 <- d2.1[-1,,drop=F]
     Delta2 <- Delta2 + d3 %*% t(a2.1) 
     Delta1 <- Delta1 + d2 %*% t(a1.1)
   }
   Theta1_grad <- matrix(0, dim(Theta1)[1], dim(Theta1)[2]) 
   Theta2_grad <- matrix(0, dim(Theta2)[1], dim(Theta2)[2]) 
   
   Theta1_grad <-  1/m * Delta1
   Theta2_grad <-  1/m * Delta2 
   
   #% Part 3: Implement regularization --------------------------------------------
   

   # % unaffect biased column...
   # Theta1_grad += lambda/m * [zeros(size(Theta1,1),1)  Theta1(:, 2:end) ];
   # Theta2_grad += lambda/m * [zeros(size(Theta2,1),1)  Theta2(:, 2:end) ];
   Theta1.tmp <- Theta1
   Theta2.tmp <- Theta2
   Theta1.tmp[,1] <- 0
   Theta2.tmp[,1] <- 0
   
   Theta1_grad <- Theta1_grad + lambda/m * Theta1.tmp
   Theta2_grad <- Theta2_grad + lambda/m * Theta2.tmp
   
   
   #% =========================================================================
       
   # % Unroll gradients
   #grad = [Theta1_grad(:) ; Theta2_grad(:)];
   grad <- c(as.vector(Theta1_grad), as.vector(Theta2_grad))
   
   grad
}




# %% ================ Part 3: Compute Cost (Feedforward) ================
#   %  To the neural network, you should first start by implementing the
# %  feedforward part of the neural network that returns the cost only. You
# %  should complete the code in nnCostFunction.m to return cost. After
# %  implementing the feedforward to compute the cost, you can verify that
# %  your implementation is correct by verifying that you get the same cost
# %  as us for the fixed debugging parameters.
# %
# %  We suggest implementing the feedforward cost *without* regularization
# %  first so that it will be easier for you to debug. Later, in part 4, you
# %  will get to implement the regularized cost.
# %
cat("\nFeedforward Using Neural Network ...\n")
# 
# % Weight regularization parameter (we set this to 0 here).
lambda = 0;

J <- nnCostFunction( nn_params, input_layer_size, hidden_layer_size, 
                                num_labels, X, y, lambda) 
# 
cat("Cost at parameters (loaded from ex4weights):", J ,
         "\n(this value should be about 0.287629)\n")


# %% =============== Part 4: Implement Regularization ===============
#   %  Once your cost function implementation is correct, you should now
# %  continue to implement the regularization with the cost.
# %

cat("\nChecking Cost Function (w/ Regularization) ... \n")

# % Weight regularization parameter (we set this to 1 here).
lambda = 1;
 
J <- nnCostFunction(nn_params, input_layer_size, hidden_layer_size,
                    num_labels, X, y, lambda)
 
cat("Cost at parameters (loaded from ex4weights):",J,
          "\n(this value should be about 0.383770)\n")
 


sigmoidGradient <- function(z){ 
    sigmoid(z) * (1 - sigmoid(z))
}



cat("\nEvaluating sigmoid gradient...\n")
g <- sigmoidGradient(c(1, -0.5, 0, 0.5, 1))
cat("Sigmoid gradient evaluated at [1 -0.5 0 0.5 1]:\n ")
cat(g)
cat("\n\n")


# %% ================ Part 6: Initializing Pameters ================
#   %  In this part of the exercise, you will be starting to implment a two
# %  layer neural network that classifies digits. You will start by
# %  implementing a function to initialize the weights of the neural network
# %  (randInitializeWeights.m)

randInitializeWeights <- function(L_in, L_out){

  # Initializing all theta weights to zero does not work with neural networks. When we backpropagate,
  # all nodes will update to the same value repeatedly.
  # 
  # Instead we can randomly initialize our weights:
  #   
  #   Initialize each Θ(l)ij to a random value between[−ϵ,ϵ]:
  #   
  #   ϵ=6√Loutput+Linput−−−−−−−−−−−−−−√

  # % Instructions: Initialize W randomly so that we break the symmetry while
  # %               training the neural network.
  # %
  # % Note: The first row of W corresponds to the parameters for the bias units
  
  epsilon_init <- sqrt(6 /(L_out + L_in + 1))
  (matrix(runif(L_out *(1+ L_in)), L_out, 1+L_in) * 2  -1 )* epsilon_init  

}


cat('\nInitializing Neural Network Parameters ...\n')
 
initial_Theta1 <- randInitializeWeights(input_layer_size, hidden_layer_size)
initial_Theta2 <- randInitializeWeights(hidden_layer_size, num_labels)

# % Unroll parameters
initial_nn_params  <- c( as.vector(initial_Theta1), as.vector(initial_Theta2))
 



# =============== Part 7: Implement Backpropagation ===============
# Once your cost matches up with ours, you should proceed to implement the
# %  backpropagation algorithm for the neural network. You should add to the
# %  code you've written in nnCostFunction.m to return the partial
# %  derivatives of the parameters.
# %
cat('\nChecking Backpropagation... \n')


debugInitializeWeights <- function(fan_out, fan_in){  
 matrix(sin(1:(fan_out * (fan_in + 1))) / 10, fan_out, 1 + fan_in) 
} 

computeNumericalGradient <- function(J, theta){
# %COMPUTENUMERICALGRADIENT Computes the gradient using "finite differences"
# %and gives us a numerical estimate of the gradient.
# %   numgrad = COMPUTENUMERICALGRADIENT(J, theta) computes the numerical
# %   gradient of the function J around theta. Calling y = J(theta) should
# %   return the function value at theta.
# 
# % Notes: The following code implements numerical gradient checking, and 
# %        returns the numerical gradient.It sets numgrad(i) to (a numerical 
# %        approximation of) the partial derivative of J with respect to the 
# %        i-th input argument, evaluated at theta. (i.e., numgrad(i) should 
# %        be the (approximately) the partial derivative of J with respect 
# %        to theta(i).)
# %                

  numgrad <- matrix(0, 1, length(theta))
  perturb <- matrix(0, 1, length(theta))
  e <- 1e-4
  
  for(p in 1:length(theta)){
    #% Set perturbation vector
    perturb[p] <- e;
    loss1 <- J(theta - perturb)
    loss2 <- J(theta + perturb)
    #% Compute Numerical Gradient
    numgrad[p] <- (loss2 - loss1) / (2*e)
    perturb[p] <- 0;
  }
  numgrad
}





checkNNGradients <-function(lambda=0){ 
# %CHECKNNGRADIENTS Creates a small neural network to check the
# %backpropagation gradients
# %   CHECKNNGRADIENTS(lambda) Creates a small neural network to check the
# %   backpropagation gradients, it will output the analytical gradients
# %   produced by your backprop code and the numerical gradients (computed
#     using computeNumericalGradient). These two gradient computations should
# %   result in very similar values.

  input_layer_size = 3;
  hidden_layer_size = 5;
  num_labels = 3;
  m = 5;
  
  # We generate some 'random' test data
  Theta1 <- debugInitializeWeights(hidden_layer_size, input_layer_size)
  Theta2 <- debugInitializeWeights(num_labels, hidden_layer_size)
  # Reusing debugInitializeWeights to generate X
  X  <- debugInitializeWeights(m, input_layer_size - 1)
  y  <- 1 + 1:m %% num_labels
  
  #% Unroll parameters
  nn_params <- c(as.vector(Theta1), as.vector(Theta2))
  
  # % Short hand for cost function
  
  costFunc <- function(p) nnCostFunction(p, input_layer_size, hidden_layer_size, 
                num_labels, X, y, lambda)
 

 
  numgrad <- computeNumericalGradient(costFunc, nn_params)
  
  grad <-nnGradFunction(nn_params, input_layer_size, hidden_layer_size, 
                        num_labels, X, y, lambda)
  
  # % Visually examine the two gradient computations.  The two columns
  # % you get should be very similar. 
  # disp([numgrad grad]);
  # fprintf(['The above two columns you get should be very similar.\n' ...
  # '(Left-Your Numerical Gradient, Right-Analytical Gradient)\n\n']);
  # 
  # % Evaluate the norm of the difference between two solutions.  
  # % If you have a correct implementation, and assuming you used EPSILON = 0.0001 
  # % in computeNumericalGradient.m, then diff below should be less than 1e-9
  diff <- norm(numgrad-grad)/norm(numgrad+grad);
  
  cat('If your backpropagation implementation is correct, then \n',
  'the relative difference will be small (less than 1e-9). \n',
  '\nRelative Difference:', diff, '\n')

}


# %  Check gradients by running checkNNGradients
checkNNGradients()


 
# %% =============== Part 8: Implement Regularization ===============
# %  Once your backpropagation implementation is correct, you should now
# %  continue to implement the regularization with the cost and gradient.
# %
# 
cat('\nChecking Backpropagation (w/ Regularization) ... \n')
 
# %  Check gradients by running checkNNGradients

lambda <- 3
checkNNGradients(lambda);
 
# % Also output the costFunction debugging values
debug_J  <- nnCostFunction(nn_params, input_layer_size,
                           hidden_layer_size, num_labels, X, y, lambda=3)[[1]]

cat('\n\nCost at (fixed) debugging parameters (w/ lambda = 3):', debug_J, 
          '\n(this value should be about 0.576051)\n\n')

##########

# %% =================== Part 8: Training NN ===================
#   %  You have now implemented all the code necessary to train a neural 
# %  network. To train your neural network, we will now use "fmincg", which
# %  is a function which works similarly to "fminunc". Recall that these
# %  advanced optimizers are able to train our cost functions efficiently as
# %  long as we provide them with the gradient computations.
# %
cat('\nTraining Neural Network... \n')

# %  After you have completed the assignment, change the MaxIter to a larger
# %  value to see how more training helps.

it <- 100
lambda <- 1

res <- optim(initial_nn_params, fn=nnCostFunction, gr=nnGradFunction, method="CG", control=list(maxit=it, type=1),
                         input_layer_size = input_layer_size, hidden_layer_size=hidden_layer_size,
                         num_labels=num_labels, X=X, y=y, lambda=lambda)


nn_params <- res$par
cost <- res$value

cat("The cost result is : ", cost, "\n")

idx.1 <- 1:(hidden_layer_size * (input_layer_size + 1))
Theta1 <- matrix(nn_params[idx.1],
                 hidden_layer_size, (input_layer_size + 1))

Theta2 <- matrix(nn_params[-idx.1],
                 num_labels, (hidden_layer_size + 1))



# 
# %% ================= Part 9: Visualize Weights =================
#   %  You can now "visualize" what the neural network is learning by 
# %  displaying the hidden units to see what features they are capturing in 
# %  the data.

cat('\nVisualizing Neural Network... \n')

displayData(Theta1[, -1])
displayData(Theta2[, -1])



# %% ================= Part 10: Implement Predict =================
#   %  After training the neural network, we would like to use it to predict
# %  the labels. You will now implement the "predict" function to use the
# %  neural network to predict the labels of the training set. This lets
# %  you compute the training set accuracy.

# Predict Function
predict <- function(Theta1, Theta2, X){
  
  m <- nrow(X)
  num_labels <- nrow(Theta2)
  
  h1 <- sigmoid(cbind(1, X) %*% t(Theta1))
  h2 <- sigmoid(cbind(1, h1) %*% t(Theta2))
  p <- matrix(apply(h2, 1, function(x) which(x==max(x))), m, 1)
  
  return(p)
}

## ================= Part 10: Implement Predict =================

pred <- predict(Theta1, Theta2, X)
accuracy <- mean(pred==y)
print(paste("The accuracy of this model is : ", accuracy))

