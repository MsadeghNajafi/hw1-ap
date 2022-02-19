#include<iostream>
#include<vector>
#include<random>
#include<iomanip>
#include<exception>
using Matrix = std::vector<std::vector<double>>;
namespace algebra
{

 Matrix zeros(size_t n,size_t m)
  {
     Matrix matrix(n,std::vector<double>(m));
     
    
     return matrix;
    
    
  }
 Matrix ones(size_t n,size_t m)
  {
      Matrix one(n,std::vector<double>(m));
      for (int i{0}; i < one.size(); i++)
      
          for (int j = 0; j < one[i].size(); j++)
          
             one[i][j]=1; 
      return one;
  }

 Matrix random(size_t n, size_t m, double min, double max)
  { 
     
     if(min<max)
     {
      Matrix one(n,std::vector<double>(m)), one2(n,std::vector<double>(m));
      for(int i{0};i<n;i++)
       {
        for (int j{0}; j < m; j++)
          {
            std::random_device rd;   // non-deterministic generator
            std::mt19937 gen(rd());  // to seed mersenne twister.
            std::uniform_real_distribution<> dist(min,max); // distribute results between 1 and 6 inclusive.
            one[i][j]=dist(gen) ;
          }
        std::cout<<"\n"; 
       }
      return one;
     }
      else
       throw std::logic_error("Error");
   
  
        
  }   
 void show(const Matrix& matrix)
  {
    for(int i{0};i<matrix.size();i++)   
      {
        for (int j{0}; j < matrix[i].size(); j++)
       
            std::cout <<std::setw(5)<< matrix[i][j] <<" ";
       
        std::cout<<"\n";
      }

  }
 Matrix multiply(const Matrix& matrix, double c)
  {   Matrix one(matrix.size(),std::vector<double>(matrix[0].size()));
      one=matrix;
      for(int i{0};i<matrix.size();i++)   
     
        for (int j{0}; j < matrix[i].size(); j++)
        {
           
           one[i][j]*=c;
        }  

      return one;
  }
 Matrix multiply(const Matrix& matrix1, const Matrix& matrix2)
  { 
     if(matrix1.empty() !=true &&   matrix1.empty()!=true && matrix1[0].size()==matrix2.size())
     {
      Matrix one1(matrix1.size(),std::vector<double>(matrix1[0].size()));
      Matrix one2(matrix2.size(),std::vector<double>(matrix2[0].size()));
      Matrix one3(matrix1.size(),std::vector<double>(matrix2[0].size()));
      
      one1=matrix1;
      one2=matrix2;
      
        for(int i{0};i<matrix1.size();i++) 
         {
          double sum{0};
          for (int k{0}; k < matrix2[0].size(); k++)
            {
               
               
                 for (int j{0};j<matrix2.size();j++)
                 {
                  
                  one3[i][k]+=one1[i][j]*one2[j][k];
                 }
                 
                  
            }
         }  
           
        return one3; 
       }
     else if(matrix1.empty() ==true &&   matrix2.empty()==true)
          return Matrix {};
     else
        throw std::logic_error("logic error");
    
      
      
           
  }
 Matrix sum(const Matrix& matrix, double c)
  {
    if(matrix.empty() !=true )
     {
      Matrix one1(matrix.size(),std::vector<double>(matrix[0].size()));
      one1=matrix;
      for(int i{0};i<matrix.size();i++)   
    
        for (int j{0}; j < matrix[i].size(); j++)

           one1[i][j]+=c;

      return one1;
    }
      else
        return Matrix{};
       

  }
 Matrix sum(const Matrix& matrix1, const Matrix& matrix2)
  {
    if(matrix1.empty() !=true &&   matrix2.empty()!=true && matrix1.size()==matrix2.size() && matrix1[0].size()==matrix2[0].size())
     {
    
      Matrix one3(matrix1.size(),std::vector<double>(matrix2[0].size()));
      
      
      for(int i{0};i<matrix1.size();i++)   
    
        for (int j{0}; j < matrix1[0].size(); j++)
         one3[i][j]=matrix1[i][j]+matrix2[i][j]; 

      return one3;
     }
    else if (matrix1.empty() ==true &&   matrix2.empty()==true)
      {
        return Matrix {};
      }
    else
      throw std::logic_error("logic error");

  }
 Matrix transpose(const Matrix& matrix)
  { if(matrix.empty() !=true)
     {
      Matrix one1(matrix[0].size(),std::vector<double>(matrix.size()));
    
      for(int j{0};j<matrix.size();j++)   
    
          for (int i{0}; i < matrix[0].size(); i++)
                
           one1[i][j]=matrix[j][i];
      return one1;
     }
    else
      return Matrix{};
          
    
  }
 Matrix minor(const Matrix& matrix, size_t n, size_t m)
  {
    Matrix one1(matrix.size()-1,std::vector<double>(matrix[0].size()-1));
    int v{0},w{0};
    for(int i{0};i<matrix.size();i++) 
      { if (i==n)
          {
             continue;
          }
        else 
          {
              for (int j = 0; j < matrix[0].size(); j++)
                {
                  if (j==m)
                   {
                    continue;
                   }
                  else
                   {
                    if(w<one1[0].size())
                     {
                       
                       one1[v][w]=matrix[i][j]; 
                       w++;
                      } 
                    else if(v<one1.size())
                     {
                       v++;
                       w=0;
                       one1[v][w]=matrix[i][j];
                       w++;
                       
                      }
                    else
                      break;
                    }
                     
                  
                }
           
          }
          
          
        
      }
    return one1;   
         



  }
 double determinant(const Matrix& matrix)
  {
   if(matrix.empty() !=true && matrix.size()==matrix[0].size())
    {
     Matrix one1(matrix.size(),std::vector<double>(matrix[0].size()));
     one1=matrix;
     double sum{0};
     if(matrix.size()==1)
        return matrix[0][0];
     else if (matrix.size()==2)
       return matrix[0][0]*matrix[1][1]-matrix[0][1]*matrix[1][0];
     else
     {
      for (size_t j = 0; j < one1[0].size(); j++)
       {
        if(j%2==0)
          sum+=one1[0][j]*determinant(minor(one1,0,j));
        else
          sum-=one1[0][j]*determinant(minor(one1,0,j));
       }
      }
     return sum;
    }
    else if(matrix.empty()==true) 
       return 1;
    else
      throw std::logic_error("logic error");
  }
 Matrix inverse(const Matrix& matrix)
  { if(matrix.empty() !=true && matrix.size()==matrix[0].size() && determinant(matrix) !=0)
     {
      Matrix one1(matrix.size(),std::vector<double>(matrix.size()));
      Matrix one2(matrix.size(),std::vector<double>(matrix.size()));
    
        for(int i{0};i<matrix.size();i++)   
    
          for (int j {0}; j <matrix[0].size(); j++)
            if((i+j)%2==0)
              one1[i][j]=determinant(minor(matrix,i,j));
            else
              one1[i][j]=-determinant(minor(matrix,i,j));
        
        one2=transpose(one1);
     
      return multiply(one2,1/(determinant(matrix)));
     }
    else if(matrix.empty()==true)
      return Matrix{};
    else  
       throw std::logic_error("logic error");

  
   
  }
 Matrix concatenate(const Matrix& matrix1, const Matrix& matrix2, int axis=0)
  {
    Matrix one1(matrix1.size(),std::vector<double>(matrix1[0].size()+matrix2[0].size()));
    Matrix one2(matrix2.size()+matrix1.size(),std::vector<double>(matrix2[0].size()));
    
    
     if( axis==1 )
     {
      
      if(matrix1.size()==matrix2.size())
       {
         for(int i{0};i<matrix1.size();i++)   
    
          for (int j{0}; j < matrix1[i].size()+matrix2[i].size(); j++)
          {
            if (j<matrix1[0].size())
         
              one1[i][j]=matrix1[i][j];
            else if (j>=matrix1[0].size())
              one1[i][j]=matrix2[i][j-matrix1[0].size()];
          }
         
          
         return one1;     
       
        }
      else
        throw std::logic_error("logic error");
     }
    
    
     
    else if(axis==0)
       {
         if(matrix1[0].size()==matrix2[0].size())
          {
           for(int j{0};j<matrix1[0].size();j++)   
    
            for (int i{0}; i < (matrix1).size()+matrix2.size(); i++)
         
             if (i<matrix1.size())
         
               one2[i][j]=matrix1[i][j];
             else if (i>=matrix1.size())
               one2[i][j]=matrix2[i-matrix1.size()][j];
        
          
           return one2; 
          }
            
         else 
              throw std::logic_error("logic error");
        }
    }
    
  
 Matrix ero_swap(const Matrix& matrix, size_t r1, size_t r2)
  { 
    if(r1< matrix.size() && r2< matrix.size())
     {
      Matrix one1(matrix.size(),std::vector<double>(matrix[0].size()));
      Matrix one2(matrix.size(),std::vector<double>(matrix[0].size()));
      Matrix one3(matrix.size(),std::vector<double>(matrix[0].size()));
      one1=matrix;
      for (size_t j{0}; j < matrix[0].size(); j++)
      {
        one1[r1][j]=matrix[r2][j];
        one1[r2][j]=matrix[r1][j];

      }
      return one1;
     }
     else
       throw std::logic_error("logic error");
    }
 Matrix ero_multiply(const Matrix& matrix, size_t r, double c)
  {
    Matrix one1(matrix.size(),std::vector<double>(matrix[0].size()));
    one1=matrix;
      
    for (size_t j{0}; j < matrix[0].size(); j++)
    {
      
      one1[r][j]=c*matrix[r][j];

    }
    return one1;
  }
 Matrix ero_sum(const Matrix& matrix, size_t r1, double c, size_t r2)
  {
    Matrix one1(matrix.size(),std::vector<double>(matrix[0].size()));
     one1=matrix;
    for (size_t j{0}; j < matrix[0].size(); j++)
    {
      one1[r2][j]+=c*matrix[r1][j];
      
    }
    return one1;

  }
 Matrix upper_triangular(const Matrix& matrix)
  { if(matrix.empty() != true && matrix.size()==matrix[0].size())
     {
      Matrix one1(matrix.size(),std::vector<double>(matrix[0].size())),ex1,ex2;
      one1=matrix;
      int sum{0};
      for(int i{0};i<matrix.size();i++)
         if(matrix[i][i]==0)
           one1=algebra::ero_swap(one1,i,i+1);

      
      ex1=ero_sum(one1,0,-one1[1][0]/one1[0][0],1);
        
      for(int j{0};j<matrix[0].size()-1;j++)
        for(int i{2};i<matrix.size();i++) 
             
          ex1=ero_sum(ex1,j,-ex1[i][j]/ex1[j][j],i);
    
      return ex1;
     }
    else if(matrix.empty()==true)
      return Matrix{};
    else
      throw std::logic_error("logic error");
  }

}
void show(const Matrix& matrix)
  {
    for(int i{0};i<matrix.size();i++)   
     {
        for (int j{0}; j < matrix[i].size(); j++)
       
            std::cout <<std::setw(5)<< matrix[i][j] <<" ";
       
        std::cout<<"\n";
     }

  }

 
